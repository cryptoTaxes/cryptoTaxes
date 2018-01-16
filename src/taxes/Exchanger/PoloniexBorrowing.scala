package taxes.Exchanger

import taxes.Util.Parse._
import taxes._

object PoloniexBorrowing extends Exchanger {
  override val id: String = "Poloniex"

  override val folder: String = "poloniex/borrowing"

  private val feesReader = new CSVSortedOperationReader {
    override val hasHeader: Boolean = true

    override def lineScanner(line: String) =
      SeparatedScanner(line, "[,%]")

    override def readLine(line: String, scLn: Scanner): Either[String, Operation] = {
      val currency = scLn.next()
      val rate = scLn.nextDouble()
      val amount = scLn.nextDouble()
      val duration = scLn.nextDouble()
      val totalFee = scLn.nextDouble()
      val open = Date.fromString(scLn.next() + " +0000", "yyyy-MM-dd HH:mm:ss Z") // Poloniex time is 1 hour behind here
      val close = Date.fromString(scLn.next() + " +0000", "yyyy-MM-dd HH:mm:ss Z") // Poloniex time is 1 hour behind here
      scLn.close()

      val desc = id + " Borrowing fees"

      val fee = Fee(
        date = close
        , id = desc
        , amount = totalFee
        , market = Market.normalize(currency)
        , exchanger = Poloniex
        , description = desc
      )
      return Right(fee)
    }
  }

  def readFile(fileName : String) : List[Operation] =
    feesReader.readFile(fileName)
}

