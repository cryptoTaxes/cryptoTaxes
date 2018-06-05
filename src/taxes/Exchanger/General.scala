package taxes.Exchanger

import taxes.Util.Parse.{CSVReader, CSVSortedOperationReader, Scanner, SeparatedScanner}
import taxes._


case class General(name : String) extends Exchanger {
  override val id = name
  override val sources = Seq()
}


object General extends Exchanger {
  override val id: String = "General"

  override val sources = Seq(
      new UserFolderSource[Operation]("general/exchanges", ".csv") {
        def fileSource(fileName : String) = exchangesReader(fileName)
      }
    , new UserFolderSource[Operation]("general/gainslosses", ".csv") {
       def fileSource(fileName : String) = gainsLossesReader(fileName)
      }
  )

  private def exchangesReader(fileName : String) = new CSVSortedOperationReader(fileName) {
    override val hasHeader: Boolean = true

    override def lineScanner(line: String) =
      SeparatedScanner(line, "[,]+")

    override def readLine(line: String, scLn: Scanner): CSVReader.Result[Operation] = {
      val date = Date.fromString(scLn.next(), "yyyy-MM-dd")
      val amount1 = scLn.nextDouble("Amount1")
      val market1 = scLn.next("Market1")
      val amount2 = scLn.nextDouble("Amount2")
      val market2 = scLn.next("Market2")
      val fee = scLn.nextDouble("Fee")
      val feeMarket = scLn.next("Fee Market")
      val exchangerName = scLn.next("Exchanger")
      val desc = scLn.next("Description")

      val exchange =
        Exchange(
          date = date
          , id = ""
          , fromAmount = amount1, fromMarket = Market.normalize(market1)
          , toAmount = amount2, toMarket = Market.normalize(market2)
          , fee = fee
          , feeMarket = Market.normalize(feeMarket)
          , exchanger = General(exchangerName)
          , description = desc
        )
      return CSVReader.Ok(exchange)
    }
  }

  private def gainsLossesReader(fileName : String) = new CSVSortedOperationReader(fileName) {
    override val hasHeader: Boolean = true

    override def lineScanner(line: String) =
      SeparatedScanner(line, "[,]+")

    override def readLine(line: String, scLn: Scanner): CSVReader.Result[Operation] = {
      val date = Date.fromString(scLn.next(), "yyyy-MM-dd")
      val amount = scLn.nextDouble("Amount1")
      val market = scLn.next("Market1")
      val fee = scLn.nextDouble("Fee")
      val feeMarket = scLn.next("Fee Market")
      val exchangerName = scLn.next("Exchanger")
      val desc = scLn.next("Description")

      val op =
        if(amount>0) {
          Gain(
              date = date
            , id = ""
            , amount = amount
            , market = market
            , exchanger = General(exchangerName)
            , description = desc
          )
        } else {
          Loss(
              date = date
            , id = ""
            , amount = amount.abs
            , market = market
            , exchanger = General(exchangerName)
            , description = desc
          )
        }

      if(fee != 0) {
        val f = Fee(
            date = date
          , id = ""
          , amount = fee
          , market = feeMarket
          , exchanger = General(exchangerName)
          , description = desc
        )
        return CSVReader.Ok(List(op, f))
      } else
        return CSVReader.Ok(op)
    }
  }
}



