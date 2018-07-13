package taxes.exchanger

import taxes._
import taxes.date._
import taxes.util.parse.{CSVReader, CSVSortedOperationReader, Scanner, SeparatedScanner}


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

  // we assume all dates are in ourm time zone
  private def parseDate(str : String) : LocalDateTime =
    LocalDateTime.parseAsMyZoneId(str+" 00:00:00", "yyyy-[MM][M]-[dd][d] HH:mm:ss")

  private def exchangesReader(fileName : String) = new CSVSortedOperationReader(fileName) {
    override val linesToSkip = 1

    override def lineScanner(line: String) =
      SeparatedScanner(line, "[,]+")

    override def readLine(line: String, scLn: Scanner): CSVReader.Result[Operation] = {
      val date = parseDate(scLn.next())
      val fromAmount = scLn.nextDouble("Amount1")
      val fromMarket = scLn.next("Market1")
      val toAmount = scLn.nextDouble("Amount2")
      val toMarket = scLn.next("Market2")
      val fee = scLn.nextDouble("Fee")
      val feeMarket = scLn.next("Fee Market")
      val exchangerName = scLn.next("Exchanger")
      val desc = scLn.next("Description")

      val exchange =
        Exchange(
          date = date
          , id = ""
          , fromAmount = fromAmount, fromMarket = Market.normalize(fromMarket)
          , toAmount = toAmount, toMarket = Market.normalize(toMarket)
          , feeAmount = fee
          , feeMarket = Market.normalize(feeMarket)
          , exchanger = General(exchangerName)
          , description = desc
        )
      return CSVReader.Ok(exchange)
    }
  }

  private def gainsLossesReader(fileName : String) = new CSVSortedOperationReader(fileName) {
    override val linesToSkip = 1

    override def lineScanner(line: String) =
      SeparatedScanner(line, "[,]+")

    override def readLine(line: String, scLn: Scanner): CSVReader.Result[Operation] = {
      val date = parseDate(scLn.next())
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



