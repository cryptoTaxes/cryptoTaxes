package taxes.Exchanger

import taxes.Util.Parse.{CSVSortedOperationReader, Scanner, SeparatedScanner}
import taxes._

object LocalBTC extends Exchanger {
  override val id: String = "LocalBTC"

  override val sources = Seq(
    new UserFolderSource[Operation]("localbitcoins") {
      def fileSource(fileName : String) = operationsReader(fileName)
    }
  )

  private def operationsReader(fileName : String) = new CSVSortedOperationReader(fileName) {
    override val hasHeader: Boolean = true

    override def lineScanner(line: String) =
      SeparatedScanner(line, "[,]")

    override def readLine(line: String, scLn: Scanner): Either[String, Operation] = {
      val orderId = scLn.next()
      val date = Date.fromOffsetString(scLn.next().replace(' ','T'))
      val buyer = scLn.next()
      val seller = scLn.next()
      val tradeType = scLn.next()
      val btc_amount = scLn.nextDouble()
      val btc_traded = scLn.nextDouble()
      val fee_btc =  scLn.nextDouble()
      val btc_amount_less_fee = scLn.nextDouble()
      val btc_final = scLn.nextDouble()
      val fiat_amount = scLn.nextDouble()
      val fiat_fee = scLn.nextDouble()
      val fiat_per_btc = scLn.nextDouble()
      val currency = scLn.next()
      val exchange_rate = scLn.nextDouble()
      val transaction_released_at = scLn.next()
      val online_provider = scLn.next()
      val reference = scLn.next()
      scLn.close()

      val desc = id + " " + orderId + "/" + reference
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

        return Right(exchange)
      } else
        return Left("%s. Read file. Reading this transaction is not currently supported: %s.".format(id, line))
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
