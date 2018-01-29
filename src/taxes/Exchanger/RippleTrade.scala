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

      val date = Date.fromString(scLn.next("Date"), "yyyy-MM-dd hh:mm:ss")
      val what = scLn.next("What")

      if (what == "Sell") {
        val amount1 = scLn.nextDouble("Amount1")
        val market1 = scLn.next("Market1")
        scLn.next("Skip")
        val amount2 = scLn.nextDouble("Amount2")
        val market2 = scLn.next("Market2")
        scLn.next("Skip")
        val price = scLn.nextDouble("Price")

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
        val amount1 = scLn.nextDouble("Amount1")
        val market1 = scLn.next("Market1")
        scLn.next("Skip")
        val amount2 = scLn.nextDouble("Amount2")
        val market2 = scLn.next("Market2")
        scLn.next("Skip")
        val price = scLn.nextDouble("Price")

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
