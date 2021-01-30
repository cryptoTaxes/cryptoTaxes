package taxes.exchanger

import taxes._
import taxes.date._
import taxes.io.FileSystem
import taxes.util._
import taxes.util.parse._

import scala.io.Source


object RippleTrade extends Exchanger {
  override val id: String = "XRPTrade"

  override val sources = {
    var srcs = Seq(
      new FilteredUserInputFolderSource[Operation]("xrptrade", ".json") {
        def fileSource(fileName: String) = new FileSource[Operation](fileName) {
          override def read(): Seq[Operation] =
            readFile(fileName)
        }
      },
      new FilteredUserInputFolderSource[Operation]("xrptrade/depositsWithdrawals", ".csv") {
        def fileSource(fileName: String) = depositsWithdrawalsReader(fileName)
      }
    )
    if(!Config.config.deprecatedUp2017Version)
      srcs ++= Seq(
        new FilteredUserInputFolderSource[Operation]("xrptrade/fees", ".csv") {
          def fileSource(fileName: String) = feesReader(fileName)
        })
    srcs
  }

  private case class Entry(hash: String, amount: Double, currency: Currency, date: LocalDateTime)

  private def readFile(fileName: String): List[Exchange] = {
    val contents = FileSystem.withSource(fileName){ src =>
      src.dropWhile(_ != '{').mkString // skip till proper start of json
    }

    val json = JsObjectAST.fromString(contents)

    val changes = json.getVector("balance_changes")

    val entries = for(change <- changes; changeJson = JsObjectAST.fromJsValue(change))
      yield {
        // txs with exactly same date are sorted according to how they changed
        // balance in XRP account so that they respect order of execution
        val order =
          scala.util.Try{
              changeJson.getDouble("final_balance") *
                1000 * // 3 digits in balance are taken into account
                changeJson.getDouble("amount_change").signum
          }.getOrElse(0.0)

        Entry(
          date = LocalDateTime.parse(changeJson.getString("executed_time"), "yyyy-MM-dd'T'HH:mm:ssX").plusNanos(order.toLong)
          , hash = changeJson.getString("tx_hash")
          , amount = Parse.asDouble(changeJson.getString("amount_change"))
          , currency = changeJson.getString("currency")
        )
      }

    var exchanges = List[Exchange]()

    val hashes = entries.map(_.hash).toSet

    for(hash <- hashes) {
      val optXRP = entries.find(entry => entry.hash == hash && entry.currency == "XRP")
      optXRP match {
        case None =>
          Logger.fatal(s"RippleTrade.readFile ${FileSystem.pathFromData(fileName)}: could not find XRP value for hash $hash")
        case Some(entryXRP) => {
          val optBTC = entries.find(entry => entry.hash == hash && entry.currency == "BTC")
          optBTC match {
            case None =>
              Logger.fatal(s"RippleTrade.readFile ${FileSystem.pathFromData(fileName)}: could not find BTC value for hash $hash")
            case Some(entryBTC) => {
              val desc = RichText(s"Order: ${RichText.transaction(Currency.ripple, hash)}")
              val feeAmount = if(Config.config.deprecatedUp2017Version) 0.012 else 0
              val exchange =
                if(entryXRP.amount < 0)
                  Exchange(
                    date = entryXRP.date
                    , id = hash
                    , fromAmount = entryXRP.amount.abs, fromCurrency = Currency.ripple
                    , toAmount = entryBTC.amount.abs, toCurrency = Currency.bitcoin
                    , fees = List(FeePair(feeAmount, Currency.ripple))
                    , exchanger = RippleTrade
                    , description = desc
                  )
                else
                  Exchange(
                    date = entryXRP.date
                    , id = hash
                    , fromAmount = entryBTC.amount.abs, fromCurrency = Currency.bitcoin
                    , toAmount = entryXRP.amount.abs, toCurrency = Currency.ripple
                    , fees = List(FeePair(feeAmount, Currency.ripple))
                    , exchanger = RippleTrade
                    , description = desc
                  )
              exchanges ::= exchange
            }
          }
        }
      }
    }
    return exchanges.sortBy(_.date)
  }

  private def depositsWithdrawalsReader(fileName: String) = new CSVSortedOperationReader(fileName) {
    override val linesToSkip = 1

    override def lineScanner(line: String): Scanner =
      SeparatedScanner(line, "[ \t]+")

    override def readLine(line: String, scLn: Scanner): CSVReader.Result[Operation] = {
      val date1 = scLn.next("Date1")
      val date2 = scLn.next("Date2")

      val fmt = "dd/MM/yyyy HH:mm"
      val date = LocalDateTime.parseAsUTC(date1 + " " + date2, fmt)

      val txHash = scLn.next("Tx Hash")
      val ledger = scLn.next("Ledger")

      val from = scLn.next("From")
      val what = scLn.next("What")
      val to = scLn.next("To")
      val amount = scLn.nextDouble("Amount")
      val currency = Currency.normalize(scLn.next("Currency"))


      if(what=="ACTIVATED" || what=="IN") {
        val desc = RichText(s"Deposit ${RichText.util.transaction(Currency.ripple, txHash, from)}")
        val deposit = Deposit(
          date = date
          , id = txHash
          , amount = amount
          , currency = currency
          , exchanger = RippleTrade
          , description = desc
        )
        return CSVReader.Ok(deposit)
      } else if(what=="OUT") {
        val desc = RichText(s"Withdrawal ${RichText.util.transaction(Currency.ripple, txHash, to)}")
        val withdrawal = Withdrawal(
          date = date
          , id = txHash
          , amount = amount
          , currency = currency
          , exchanger = RippleTrade
          , description = desc
        )
        return CSVReader.Ok(withdrawal)

      } else
        CSVReader.Warning(s"$id. Read deposit/withdrawal ${FileSystem.pathFromData(fileName)}: This line could not be read: $line.")
    }
  }

  private def feesReader(fileName: String) = new CSVSortedOperationReader(fileName) {
    override val linesToSkip = 1

    override def lineScanner(line: String): Scanner =
      QuotedScanner(line, '"', ',')

    override def readLine(line: String, scLn: Scanner): CSVReader.Result[Operation] = {
      val time = scLn.next("Time")
      val rawTime = scLn.next("Raw Time")
      val _type = scLn.next("Type")
      val amount = scLn.nextDouble("Amount")
      val currency = Currency.normalize(scLn.next("Currency"))
      val counterparty = scLn.next("Counterparty")
      val counterpartyName = scLn.next("Counterparty Name")
      val balance = scLn.next("Balance")
      val hash = scLn.next("Hash")

      val date = LocalDateTime.parse(rawTime, "yyyy-MM-dd'T'HH:mm:ss.SSSX").minusNanos(1)

      if(_type=="transaction_cost") {
        val desc = RichText(s"$id fee ${RichText.util.transaction(Currency.ripple, hash)}")
        val fee = {
          if(Config.config.fundingFees)
            Fee(
              date = date
              , id = hash
              , amount = amount
              , currency = currency
              , exchanger = RippleTrade
              , description = RichText(s"$id fee ${RichText.util.transaction(Currency.ripple, hash)}")
            )
          else
            NonTaxableFee(
              date = date
              , id = hash
              , amount = amount.abs
              , currency = currency
              , exchanger = RippleTrade
              , description = RichText(s"$id non taxable fee ${RichText.util.transaction(Currency.ripple, hash)}")
            )
        }
        CSVReader.Ok(fee)
      } else
        CSVReader.Warning(s"$id. Read deposit/withdrawal ${FileSystem.pathFromData(fileName)}: This line could not be read: $line.")
    }
  }
}
