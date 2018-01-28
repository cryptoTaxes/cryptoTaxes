package taxes.Exchanger

import taxes.Util.Parse.{CSVReader, CSVSortedOperationReader, Scanner, SeparatedScanner}
import taxes._

object GDAX extends Exchanger {
  override val id: String = "GDAX"

  override val sources = Seq(
    new UserFolderSource[Operation]("gdax") {
      def fileSource(fileName : String) = operationsReader(fileName)
    }
  )

  private def operationsReader(fileName : String) = new CSVSortedOperationReader(fileName) {
    override val hasHeader: Boolean = true

    override def lineScanner(line: String): Scanner =
      SeparatedScanner(line, "[,]")

    override def readLine(line: String, scLn: Scanner): CSVReader.Result[Operation] = {
      val tradeID = scLn.next()
      val product = scLn.next()
      val side = scLn.next()
      val createdAt = scLn.next()
      val size = scLn.nextDouble()
      val sizeUnit = scLn.next()
      val price = scLn.nextDouble()
      val fee = scLn.nextDouble()
      val total = scLn.nextDouble()
      val priceFeeTotalUnit = scLn.next()
      scLn.close()

      val desc = GDAX + " " + tradeID
      val date = Date.fromString(createdAt, "yyyy-MM-dd'T'HH:mm:ss.SSSX")

      val (market1, aux) = product.span(_ != '-')
      val market2 = aux.tail

      if (side == "SELL") {
        val exchange =
          Exchange(
            date = date
            , id = tradeID
            , fromAmount = size, fromMarket = Market.normalize(sizeUnit)
            , toAmount = total, toMarket = Market.normalize(priceFeeTotalUnit)
            , fee = fee, feeMarket = Market.normalize(priceFeeTotalUnit)
            , exchanger = GDAX
            , description = desc
          )
        return CSVReader.Ok(exchange)
      } else if (side == "BUY") {
        val exchange =
          Exchange(
            date = date
            , id = tradeID
            , fromAmount = total.abs, fromMarket = Market.normalize(priceFeeTotalUnit)
            , toAmount = size, toMarket = Market.normalize(sizeUnit)
            , fee = fee, feeMarket = Market.normalize(priceFeeTotalUnit)
            , exchanger = GDAX
            , description = desc
          )
        return CSVReader.Ok(exchange)
      } else
        return CSVReader.Warning("%s. Read file: Reading this transaction is not currently supported: %s.".format(id, line))
    }
  }
}
