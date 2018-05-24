package taxes.Exchanger

import taxes.Util.Parse.{CSVReader, CSVSortedOperationReader, Scanner, SeparatedScanner}
import taxes._

object LocalBTC extends Exchanger {
  override val id: String = "LocalBTC"

  override val sources = Seq(
    new UserFolderSource[Operation]("localbitcoins", ".csv") {
      def fileSource(fileName : String) = operationsReader(fileName)
    }
  )

  private def operationsReader(fileName : String) = new CSVSortedOperationReader(fileName) {
    override val hasHeader: Boolean = true

    override def lineScanner(line: String) =
      SeparatedScanner(line, "[,]")

    override def readLine(line: String, scLn: Scanner): CSVReader.Result[Operation] = {
      val orderId = scLn.next("Order ID")
      val date = Date.fromOffsetString(scLn.next("Date").replace(' ','T'))
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
      val transaction_released_at = scLn.next("Transaction Released At")
      val online_provider = scLn.next("Online Provider")
      val reference = scLn.next("Reference")
      scLn.close()

      val desc = "Order: " + orderId + "/" + reference
      if(tradeType=="ONLINE_SELL") {
        val exchange = Exchange(
          date = date
          , id = orderId
          , fromAmount = fiat_amount, fromMarket = Market.normalize(currency)
          , toAmount = btc_amount_less_fee, toMarket = Market.bitcoin
          , fee = fee_btc, feeMarket = Market.bitcoin
          , exchanger = LocalBTC
          , description = desc
        )

        return CSVReader.Ok(exchange)
      } else
        return CSVReader.Warning("%s. Read file %s: Reading this transaction is not currently supported: %s.".format(id, Paths.pathFromData(fileName), line))
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
}
