package taxes.exchanger

import taxes._
import taxes.date._
import taxes.io.FileSystem
import taxes.util.parse.{CSVReader, CSVSortedOperationReader, Scanner, SeparatedScanner}


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

    override def lineScanner(line: String) =
      SeparatedScanner(line, "[,]")

    override def readLine(line: String, scLn: Scanner): CSVReader.Result[Operation] = {
      val orderId = scLn.next("id")
      val date = LocalDateTime.parseAsUTC(
        scLn.next("created_at")
        , "yyyy-MM-dd HH:mm:ssXXX"
      )
      val buyer = scLn.next("buyer")
      val seller = scLn.next("seller")
      val tradeType = scLn.next("trade_type")
      val btc_amount = scLn.nextDouble("btc_amount")
      val btc_traded = scLn.nextDouble("btc_traded")
      val fee_btc =  scLn.nextDouble("fee_btc")
      val btc_amount_less_fee = scLn.nextDouble("btc_amount_less_fee")
      val btc_final = scLn.nextDouble("btc_final")
      val fiat_amount = scLn.nextDouble("fiat_amount")
      val fiat_fee = scLn.nextDouble("fiat_fee")
      val fiat_per_btc = scLn.nextDouble("fiat_per_btc")
      val currency = Currency.normalize(scLn.next("currency"))
      val exchange_rate = scLn.nextDouble("exchange_rate")
      val transaction_released_at = LocalDateTime.parseAsUTC(
        scLn.next("transaction_released_at")
        , "yyyy-MM-dd HH:mm:ssXXX"
        )
      val online_provider = scLn.next("online_provider")
      val reference = scLn.next("reference")

      val desc = "Order: " + orderId + "/" + reference
      if(tradeType=="ONLINE_SELL") { // ONLINE_SELL is really a buy
        val deposit = Deposit(  // FIAT deposit for buying
          date = date
          , id = orderId
          , amount = fiat_amount
          , currency = currency
          , exchanger = LocalBTC
          , description = RichText(s"Deposit: $orderId / $reference")
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

    override def lineScanner(line: String) =
      SeparatedScanner(line, "[,]")

    override def readLine(line: String, scLn: Scanner): CSVReader.Result[Operation] = {
      //val orderId = scLn.next("TXID")
      val created = LocalDateTime.parseAsUTC(scLn.next("Created"), "yyyy-MM-dd'T'HH:mm:ssXXX")
      val receivedStr = scLn.next("Received")
      val sentStr = scLn.next("Sent")
      val txType = scLn.next("TXtype")
      val txDesc = scLn.next("TXdesc")
      val txNotes = scLn.nextDouble("TXnotes")

      if(txType=="Withdraw") {
        val amount = util.parse.Parse.asDouble(sentStr)

        val opt = try {
          val address = scLn.next("address")
          val txHash = scLn.next("txHash")
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
              , amount = util.parse.Parse.asDouble(sentStr)
              , currency = Currency.bitcoin
              , exchanger = LocalBTC
              , description = desc
            )
            withdraw

          case None =>
            val fee =
              if (Config.config.fundingFees)
                Fee(
                  date = created
                  , id = ""
                  , amount = util.parse.Parse.asDouble(sentStr)
                  , currency = Currency.bitcoin
                  , exchanger = LocalBTC
                  , description = RichText(s"$id withdrawal fee")
                )
              else
                NonTaxableFee(
                  date = created
                  , id = ""
                  , amount = util.parse.Parse.asDouble(sentStr)
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
