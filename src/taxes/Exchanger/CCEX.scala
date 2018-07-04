package taxes.Exchanger

import taxes.Util.Parse.{CSVReader, CSVSortedOperationReader, Scanner, SeparatedScanner}
import taxes._

object CCEX extends Exchanger {

  override val id: String = "C-CEX"

  override val sources = Seq(
    new UserFolderSource[Operation]("c-cex", ".csv") {
      def fileSource(fileName : String) = operationsReader(fileName)
    }
  )

  private def operationsReader(fileName : String) = new CSVSortedOperationReader(fileName) {
    override val linesToSkip = 1

    override def lineScanner(line: String) =
      SeparatedScanner(line, "[ \t]+")

    override def readLine(line: String, scLn: Scanner) : CSVReader.Result[Operation] = {
      val token1 = scLn.next("Date1")
      val token2 = scLn.next("Date2")
      val orderType = scLn.next("Order Type")

      if(orderType=="Transaction") {
        val date = Date.fromString(token1+" "+token2, "yyyy-MM-dd HH:mm:ss")
        val amount1 = scLn.nextDouble("Amount1")
        val m1 = scLn.next("Market1")
        val amount2 = scLn.nextDouble("Amount2")
        val m2 = scLn.next("Market2")
        scLn.close()

        val market1 = Market.normalize(m1)
        val market2 = Market.normalize(m2)

        val feePercent = 0.2

        val fee = amount1 * feePercent / 100

        val exchange =
          Exchange(
            date = date
            , id = ""
            , fromAmount = amount2, fromMarket = market2
            , toAmount = amount1, toMarket = market1  // amount1 in read csv doesn't include fee
            , feeAmount = fee
            , feeMarket = market1
            , exchanger = CCEX
            , description = ""
          )

        return CSVReader.Ok(exchange)
      } else
        return CSVReader.Warning("%s. Read file %s: Reading this transaction is not currently supported: %s.".format(id, Paths.pathFromData(fileName), line))
    }
  }
}

