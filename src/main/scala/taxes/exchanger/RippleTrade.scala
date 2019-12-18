package taxes.exchanger

import taxes._
import taxes.date._
import taxes.io.FileSystem
import taxes.util._
import taxes.util.parse._

import scala.io.Source


object RippleTrade extends Exchanger {
  override val id: String = "XRPTrade"

  override val sources = Seq(
    new UserInputFolderSource[Operation]("xrptrade", ".json") {
      def fileSource(fileName: String) = new FileSource[Operation](fileName) {
        override def read(): Seq[Operation] =
          readFile(fileName)
      }
    },
    new UserInputFolderSource[Operation]("xrptrade/depositsWithdrawals", ".csv") {
      def fileSource(fileName: String) = depositsWithdrawalsReader(fileName)
    }

  )

  private case class Entry(hash: String, amount: Double, currency: Market, date: LocalDateTime)

  private def readFile(fileName: String): List[Exchange] = {
    val contents = FileSystem.withSource(fileName){ src =>
      src.dropWhile(_ != '{').mkString // skip till proper start of json
    }

    val json = JsObjectAST.fromString(contents)

    val changes = json.getVector("balance_changes")

    val entries = for(change <- changes; changeJson = JsObjectAST.fromJsValue(change))
      yield
        Entry(
          date = LocalDateTime.parse(changeJson.getString("executed_time"), "yyyy-MM-dd'T'HH:mm:ssX")
          , hash = changeJson.getString("tx_hash")
          , amount = Parse.asDouble(changeJson.getString("amount_change"))
          , currency = changeJson.getString("currency")
        )

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
              val desc = "Order: " + hash
              val exchange =
                if(entryXRP.amount < 0)
                  Exchange(
                    date = entryXRP.date
                    , id = hash
                    , fromAmount = entryXRP.amount.abs, fromMarket = Market.ripple
                    , toAmount = entryBTC.amount.abs, toMarket = Market.bitcoin
                    , fees = List(FeePair(0.012, Market.ripple))
                    , exchanger = RippleTrade
                    , description = desc
                  )
                else
                  Exchange(
                    date = entryXRP.date
                    , id = hash
                    , fromAmount = entryBTC.amount.abs, fromMarket = Market.bitcoin
                    , toAmount = entryXRP.amount.abs, toMarket = Market.ripple
                    , fees = List(FeePair(0.012, Market.ripple))
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
      val market = Market.normalize(scLn.next("Market"))


      if(what=="ACTIVATED" || what=="IN") {
        val desc = "Deposit " + from + "\n" + txHash
        val deposit = Deposit(
          date = date
          , id = txHash
          , amount = amount
          , market = market
          , exchanger = RippleTrade
          , description = desc
        )
        return CSVReader.Ok(deposit)
      } else if(what=="OUT") {
        val desc = "Withdrawal " + AddressBook.format(to) + "\n" + txHash
        val withdrawal = Withdrawal(
          date = date
          , id = txHash
          , amount = amount
          , market = market
          , exchanger = RippleTrade
          , description = desc
        )
        return CSVReader.Ok(withdrawal)

      } else
        CSVReader.Warning(s"$id. Read deposit/withdrawal ${FileSystem.pathFromData(fileName)}: This line could not be read: $line.")
    }
  }

}
