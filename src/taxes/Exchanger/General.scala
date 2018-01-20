package taxes.Exchanger

import sun.java2d.loops.FillRect
import taxes.Util.Parse.{CSVReader, CSVSortedOperationReader, Scanner, SeparatedScanner}
import taxes._

object General extends Exchanger {
  override val id: String = "General"

  override val sources = Seq(
    new UserFolderSource[Operation]("general") {
      def fileSource(fileName : String) = operationsReader(fileName)
    }
  )

  private def operationsReader(fileName : String) = new CSVSortedOperationReader(fileName) {
    override val hasHeader: Boolean = true

    override def lineScanner(line: String) =
      SeparatedScanner(line, "[,]+")

    override def readLine(line: String, scLn: Scanner): CSVReader.Result[Operation] = {
      val date = Date.fromString(scLn.next(), "yyyy-MM-dd")
      val amount1 = scLn.nextDouble()
      val market1 = scLn.next()
      val amount2 = scLn.nextDouble()
      val market2 = scLn.next()
      val fee = scLn.nextDouble()
      val feeMarket = scLn.next
      val desc = scLn.next()
      scLn.close()

      val exchange =
        Exchange(
          date = date
          , id = ""
          , fromAmount = amount1, fromMarket = Market.normalize(market1)
          , toAmount = amount2, toMarket = Market.normalize(market2)
          , fee = fee
          , feeMarket = Market.normalize(feeMarket)
          , exchanger = General
          , description = General + " " + desc
        )
      return CSVReader.Ok(exchange)
    }
  }
}



