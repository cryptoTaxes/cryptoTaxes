package taxes.Exchanger

import taxes.Util.Parse.{CSVSortedOperationReader, Scanner, SeparatedScanner}
import taxes.{Date, Exchange, Market, Operation}

object General extends Exchanger {
  override val id: String = "General"

  override val folder: String = "general"

  private val operationsReader = new CSVSortedOperationReader {
    override val hasHeader: Boolean = true

    override def lineScanner(line: String) =
      SeparatedScanner(line, "[,]+")

    override def readLine(line: String, scLn: Scanner): Either[String, Operation] = {
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
          , description = id+ " "+desc
        )
      return Right(exchange)
    }
  }

  def readFile(fileName: String) : List[Operation] =
    operationsReader.readFile(fileName)
}



