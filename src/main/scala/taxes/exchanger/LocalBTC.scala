package taxes.exchanger

import taxes._
import taxes.date._
import taxes.io.FileSystem
import taxes.util.parse.{AssociativeQuotedScannerProvider, AssociativeSeparatedScannerProvider, CSVReader, CSVSortedOperationReader, Scanner, SeparatedScanner}


object LocalBTC extends Exchanger {
  override val id: String = "LocalBTC"

  override val sources = Seq(
    new FilteredUserInputFolderSource[Operation]("localbitcoins", ".csv") {
      def fileSource(fileName: String) = operationsReader(fileName)
    },
    new FilteredUserInputFolderSource[Operation]("localbitcoins/withdrawals", ".csv") {
      def fileSource(fileName: String) = withdrawalsReader(fileName)
    }
  )

  private def operationsReader(fileName: String) = new CSVSortedOperationReader(fileName) {
    override val linesToSkip = 1

    private lazy val provider = AssociativeSeparatedScannerProvider(skippedLines(0), "[,]")
    override def lineScanner(line: String) =
      provider.scannerFor(line)

    override def readLine(line: String, scLn: Scanner): CSVReader.Result[Operation] = {
      val orderId = scLn.next("id")
      val date = LocalDateTime.parseAsUTC(
        scLn.next("created_at")
        , "yyyy-MM-dd HH:mm:ssXXX"
      )
      val tradeType = scLn.next("trade_type")
      val fee_btc =  scLn.nextDouble("fee_btc")
      val btc_amount_less_fee = scLn.nextDouble("btc_amount_less_fee")
      val fiat_amount = scLn.nextDouble("fiat_amount")
      val currency = Currency.normalize(scLn.next("currency"))
      val transaction_released_at = LocalDateTime.parseAsUTC(
        scLn.next("transaction_released_at")
        , "yyyy-MM-dd HH:mm:ssXXX"
        )
      val reference = scLn.next("reference")

      if(tradeType=="ONLINE_SELL") { // ONLINE_SELL is really a buy
        val deposit = Deposit(  // FIAT deposit for buying
          date = date
          , id = orderId
          , amount = fiat_amount
          , currency = currency
          , exchanger = LocalBTC
          , address = None
          , txid = Some(s"$orderId / $reference")
          , description = RichText(s"Deposit: ${RichText.small(s"$orderId / $reference")}")
        )

        val exchange = Exchange(
          date = transaction_released_at
          , id = orderId
          , fromAmount = fiat_amount, fromCurrency = currency
          , toAmount = btc_amount_less_fee, toCurrency = Currency.bitcoin
          , fees = List(FeePair(fee_btc, Currency.bitcoin))
          , exchanger = LocalBTC
          , description = RichText(s"Order: $orderId / $reference")
        )

        return CSVReader.Ok(List(deposit,exchange))
      } else
        // toDo support sells at LocalBTC
        return CSVReader.Warning(s"$id. Read file ${FileSystem.pathFromData(fileName)}: Reading this transaction is not currently supported: $line.")
    }
  }

  private def withdrawalsReader(fileName: String) = new CSVSortedOperationReader(fileName) {
    override val linesToSkip = 1

    private lazy val provider = AssociativeQuotedScannerProvider(skippedLines(0), '\"', ',')
    override def lineScanner(line: String) =
      provider.scannerFor(line)

    override def readLine(line: String, scLn: Scanner): CSVReader.Result[Operation] = {
      val created = LocalDateTime.parseAsUTC(scLn.next("Created"), "yyyy-MM-dd'T'HH:mm:ssXXX")
      val sent = scLn.next("Sent")
      val txType = scLn.next("TXtype")

      if(txType=="Withdraw") {
        val amount = util.parse.Parse.asDouble(sent)

        val opt = try {
          val address = scLn.next("Address")
          val txHash = scLn.next("Hash")
          Some((address, txHash))
        } catch {
          case _:Exception =>
            None
        }

        val operation = opt match {
          case Some((address, txHash)) =>
            val desc = RichText(s"Withdrawal ${RichText.util.transaction(Currency.bitcoin, txHash, address)}")
            val withdraw = Withdrawal(
              date = created
              , id = ""
              , amount = amount
              , currency = Currency.bitcoin
              , exchanger = LocalBTC
              , address = Some(address)
              , txid = Some(txHash)
              , description = desc
            )
            withdraw

          case None =>
            val fee =
              if (Config.config.fundingFees)
                Fee(
                  date = created
                  , id = ""
                  , amount = amount
                  , currency = Currency.bitcoin
                  , exchanger = LocalBTC
                  , description = RichText(s"$id withdrawal fee")
                )
              else
                NonTaxableFee(
                  date = created
                  , id = ""
                  , amount = amount
                  , currency = Currency.bitcoin
                  , exchanger = LocalBTC
                  , description = RichText(s"$id withdrawal fee")
                )
            fee
        }
        return CSVReader.Ok(operation)
      } else
         return CSVReader.Warning(s"$id. Read file ${FileSystem.pathFromData(fileName)}: Reading this line is not currently supported: $line.")
      }
    }
}
