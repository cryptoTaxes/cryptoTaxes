package taxes.exchanger

import taxes._
import taxes.date._
import taxes.util.Logger
import taxes.util.parse._

object GDAX extends Exchanger {
  override val id: String = "GDAX"

  override val sources = Seq(
    new UserFolderSource[Operation]("gdax", ".csv") {
      def fileSource(fileName : String) = operationsReader(fileName)
    }
  )

  private def operationsReader(fileName : String) = new CSVSortedOperationReader(fileName) {
    override val linesToSkip = 1

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
      val date = LocalDateTime.parse(createdAt, "yyyy-MM-dd'T'HH:mm:ss.SSSX") // GDAX includes a zone-offset 'Z' at the end

      val (baseMarket_0, quoteMarket_0) = Parse.split(product, "-")
      val baseMarket = Market.normalize(baseMarket_0)
      val quoteMarket = Market.normalize(quoteMarket_0)

      val sizeMarket = Market.normalize(sizeUnit)
      val priceFeeTotalMarket = Market.normalize(priceFeeTotalUnit)

      if(sizeMarket != baseMarket)
        return CSVReader.Warning("%s. Read file %s: Reading this transaction is not currently supported: %s\nas sizeMarket(%s) and quoteMarket(%s) are different.".format(id, Paths.pathFromData(fileName), line, sizeMarket, quoteMarket))

      if (side == "SELL") {
        val exchange =
          Exchange(
            date = date
            , id = tradeID
            , fromAmount = size, fromMarket = baseMarket
            , toAmount = total, toMarket = quoteMarket
            , feeAmount = fee, feeMarket = priceFeeTotalMarket
            , exchanger = GDAX
            , description = desc
          )
        return CSVReader.Ok(exchange)
      } else if (side == "BUY") {
        val exchange =
          Exchange(
            date = date
            , id = tradeID
            , fromAmount = total.abs, fromMarket = quoteMarket
            , toAmount = size, toMarket = baseMarket
            , feeAmount = fee, feeMarket = priceFeeTotalMarket
            , exchanger = GDAX
            , description = desc
          )
        return CSVReader.Ok(exchange)
      } else
        return CSVReader.Warning("%s. Read file %s: Reading this transaction is not currently supported: %s.".format(id, Paths.pathFromData(fileName), line))
    }
  }
}
