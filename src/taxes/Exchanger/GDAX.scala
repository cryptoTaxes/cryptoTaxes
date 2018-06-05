package taxes.Exchanger

import taxes.Util.Parse.{CSVReader, CSVSortedOperationReader, Scanner, SeparatedScanner}
import taxes._

object GDAX extends Exchanger {
  override val id: String = "GDAX"

  override val sources = Seq(
    new UserFolderSource[Operation]("gdax", ".csv") {
      def fileSource(fileName : String) = operationsReader(fileName)
    }
  )

  private def operationsReader(fileName : String) = new CSVSortedOperationReader(fileName) {
    override val hasHeader: Boolean = true

    override def lineScanner(line: String): Scanner =
      SeparatedScanner(line, "[,]")

    override def readLine(line: String, scLn: Scanner): CSVReader.Result[Operation] = {
      val tradeID = scLn.next("Trade ID")
      val product = scLn.next("Product")
      val side = scLn.next("Side")
      val createdAt = scLn.next("Created At")
      val size = scLn.nextDouble("Size")
      val sizeUnit = scLn.next("Size Unit")
      val price = scLn.nextDouble("Price")
      val fee = scLn.nextDouble("Fee")
      val total = scLn.nextDouble("Total")
      val priceFeeTotalUnit = scLn.next("Price/Total Unit")
      scLn.close()

      val desc = "Order: " + tradeID
      val date = Date.fromString(createdAt, "yyyy-MM-dd'T'HH:mm:ss.SSSX")

      val (market1, aux) = product.span(_ != '-')
      val market2 = aux.tail

      val sizeMarket = Market.normalize(sizeUnit)
      val priceFeeTotalMarket = Market.normalize(priceFeeTotalUnit)

      if (side == "SELL") {
        val exchange =
          Exchange(
            date = date
            , id = tradeID
            , fromAmount = size, fromMarket = sizeMarket
            , toAmount = total, toMarket = priceFeeTotalMarket
            , fee = fee, feeMarket = priceFeeTotalMarket
            , exchanger = GDAX
            , description = desc
          )
        return CSVReader.Ok(exchange)
      } else if (side == "BUY") {
        val exchange =
          Exchange(
            date = date
            , id = tradeID
            , fromAmount = total.abs, fromMarket = priceFeeTotalMarket
            , toAmount = size, toMarket = sizeMarket
            , fee = fee, feeMarket = priceFeeTotalMarket
            , exchanger = GDAX
            , description = desc
          )
        return CSVReader.Ok(exchange)
      } else
        return CSVReader.Warning("%s. Read file %s: Reading this transaction is not currently supported: %s.".format(id, Paths.pathFromData(fileName), line))
    }
  }
}
