package taxes.exchanger

import taxes._
import taxes.date._
import taxes.io.FileSystem
import taxes.util.parse.{CSVReader, CSVSortedOperationReader, Scanner, SeparatedScanner}


object LocalBTC extends Exchanger {
  override val id: String = "LocalBTC"

  override val sources = Seq(
    new UserInputFolderSource[Operation]("localbitcoins", ".csv") {
      def fileSource(fileName: String) = operationsReader(fileName)
    },
    new UserInputFolderSource[Operation]("localbitcoins/withdrawals", ".csv") {
      def fileSource(fileName: String) = withdrawalsReader(fileName)
    }
  )

  private def operationsReader(fileName: String) = new CSVSortedOperationReader(fileName) {
    override val linesToSkip = 1

    override def lineScanner(line: String) =
      SeparatedScanner(line, "[,]")

    override def readLine(line: String, scLn: Scanner): CSVReader.Result[Operation] = {
      val orderId = scLn.next("Order ID")
      val date = scLn.next("Date")
      val buyer = scLn.next("Buyer")
      val seller = scLn.next("Seller")
      val tradeType = scLn.next("Trade Type")
      val btc_amount = scLn.nextDouble("BTC Amount")
      val btc_traded = scLn.nextDouble("BTC Traded")
      val fee_btc =  scLn.nextDouble("Fee BTC")
      val btc_amount_less_fee = scLn.nextDouble("BTC Less Fee")
      val btc_final = scLn.nextDouble("BTC Final")
      val fiat_amount = scLn.nextDouble("FIAT Amount")
      val fiat_fee = scLn.nextDouble("FIAT Fee")
      val fiat_per_btc = scLn.nextDouble("FIAT Per BTC")
      val currency = scLn.next("Currency")
      val exchange_rate = scLn.nextDouble("Exchange Rate")
      val transaction_released_at = LocalDateTime.parseAsUTC(
        scLn.next("Transaction Released At")
        , "yyyy-MM-dd HH:mm:ssXXX"
        )
      val online_provider = scLn.next("Online Provider")
      val reference = scLn.next("Reference")

      val desc = "Order: " + orderId + "/" + reference
      if(tradeType=="ONLINE_SELL") { // ONLINE_SELL is really a buy
        val exchange = Exchange(
          date = transaction_released_at
          , id = orderId
          , fromAmount = fiat_amount, fromMarket = Market.normalize(currency)
          , toAmount = btc_amount_less_fee, toMarket = Market.bitcoin
          , fees = List(FeePair(fee_btc, Market.bitcoin))
          , exchanger = LocalBTC
          , description = desc
        )

        return CSVReader.Ok(exchange)
      } else
        // toDo support sells at LocalBTC
        return CSVReader.Warning(s"$id. Read file ${FileSystem.pathFromData(fileName)}: Reading this transaction is not currently supported: $line.")
      /*
          Exchange(
            date = date
            , fromAmount = btc_amount, fromMarket = Market.bitcoin
            , toAmount = fiat_amount, toMarket = Market.normalize(currency)
            , fee = fiat_fee, feeMarket = Market.normalize(currency)
            , description = desc
          )
       */
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
        val desc = "Withdrawal"
        val withdraw = Withdrawal(
          date = created
          , id = ""
          , amount = util.parse.Parse.asDouble(sentStr)
          , market = Market.bitcoin
          , exchanger = LocalBTC
          , description = desc
        )
        return CSVReader.Ok(withdraw)
      } else
        return CSVReader.Warning(s"$id. Read file ${FileSystem.pathFromData(fileName)}: Reading this line is not currently supported: $line.")
      }
    }
}
