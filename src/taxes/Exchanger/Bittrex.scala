package taxes.Exchanger

import taxes.Util.Parse.{CSVReader, CSVSortedOperationReader, Scanner, SeparatedScanner}
import taxes._

object Bittrex extends Exchanger {
  override val id: String = "Bittrex"

  override val sources = Seq(
    new UserFolderSource[Operation]("bittrex") {
      def fileSource(fileName : String) = operationsReader(fileName)
    }
  )

  private def operationsReader(fileName : String) = new CSVSortedOperationReader(fileName) {
    override val hasHeader: Boolean = true

    override def lineScanner(line: String) =
      SeparatedScanner(line, "[,]")

    override def readLine(line: String, scLn: Scanner) : CSVReader.Result[Operation] = {
      val orderId = scLn.next()
      val (market1, aux) = scLn.next().span(_ != '-')
      val market2 = aux.tail

      val isSell = scLn.next() == "LIMIT_SELL"
      val quantity = scLn.nextDouble()
      val limit = scLn.nextDouble()
      val comissionPaid = scLn.nextDouble()

      val price = scLn.nextDouble()

      val dateOpen = Date.fromString(scLn.next()+" +0000", "MM/dd/yyyy hh:mm:ss a Z") // Bittrex time is 1 hour behind here
      val dateClose = Date.fromString(scLn.next()+" +0000", "MM/dd/yyyy hh:mm:ss a Z")

      val desc = id + " " + orderId

      // fees are denominated in market1. market1 is normally BTC, USDT or ETH
      val exchange =
        if (isSell)
          Exchange(
            date = dateClose
            , id = orderId
            , fromAmount = quantity, fromMarket = Market.normalize(market2)
            , toAmount = price - comissionPaid, toMarket = Market.normalize(market1)
            , fee = comissionPaid, feeMarket = Market.normalize(market1)
            , exchanger = Bittrex
            , description = desc
          )
        else
          Exchange(
            date = dateClose
            , id = orderId
            , fromAmount = price, fromMarket = Market.normalize(market1)
            , toAmount = quantity, toMarket = Market.normalize(market2)
            , fee = comissionPaid, feeMarket = Market.normalize(market1)
            , exchanger = Bittrex
            , description = desc
          )
      return CSVReader.Ok(exchange)
    }
  }
}
