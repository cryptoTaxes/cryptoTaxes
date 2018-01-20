package taxes.Exchanger

import taxes.Util.Parse._
import taxes._

object RippleTrade extends Exchanger {
  override val id: String = "XRP Trade"

  override val sources = Seq(
    new UserFolderSource[Operation]("xrptrade") {
      def fileSource(fileName : String) = operationsReader(fileName)
    }
  )

  private def operationsReader(fileName : String) = new CSVSortedOperationReader(fileName) {
    override val hasHeader: Boolean = false

    override def lineScanner(line: String): Scanner =
      SeparatedScanner(line, "[,]")

    override def readLine(line: String, scLn: Scanner): CSVReader.Result[Operation] = {
      val desc = RippleTrade.toString

      val date = Date.fromString(scLn.next(), "yyyy-MM-dd hh:mm:ss")
      val what = scLn.next()

      if (what == "Sell") {
        val amount1 = scLn.nextDouble()
        val market1 = scLn.next()
        scLn.next()
        val amount2 = scLn.nextDouble()
        val market2 = scLn.next()
        scLn.next()
        val price = scLn.nextDouble()

        val exchange =
          Exchange(
            date = date
            , id = ""
            , fromAmount = amount1, fromMarket = Market.normalize(market1)
            , toAmount = amount2, toMarket = Market.normalize(market2)
            , fee = 0, feeMarket = Market.normalize(market2)
            , exchanger = RippleTrade
            , description = desc
          )
        return CSVReader.Ok(exchange)
      } else if (what == "Buy") {
        val amount1 = scLn.nextDouble()
        val market1 = scLn.next()
        scLn.next()
        val amount2 = scLn.nextDouble()
        val market2 = scLn.next()
        scLn.next()
        val price = scLn.nextDouble()

        val exchange =
          Exchange(
            date = date
            , id = ""
            , fromAmount = amount2, fromMarket = Market.normalize(market2)
            , toAmount = amount1, toMarket = Market.normalize(market1)
            , fee = 0, feeMarket = Market.normalize(market1)
            , exchanger = RippleTrade
            , description = desc
          )
        return CSVReader.Ok(exchange)
      } else
        return CSVReader.Warning("%s. Read file: Reading this transaction is not currently supported: %s.".format(id, line))
    }
  }
}
