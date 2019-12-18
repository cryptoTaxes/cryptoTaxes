package taxes.exchanger

import taxes._
import taxes.date._
import taxes.util.parse.{CSVReader, CSVSortedOperationReader, Scanner, SeparatedScanner}


case class General(name: String) extends Exchanger {
  override val id = name
  override val sources = Seq()
}


object General extends Exchanger {
  override val id: String = "General"

  override val sources = Seq(
      new UserInputFolderSource[Operation]("general/exchanges", ".csv") {
        def fileSource(fileName: String) = exchangesReader(fileName)
      }
    , new UserInputFolderSource[Operation]("general/gainslosses", ".csv") {
       def fileSource(fileName: String) = gainsLossesReader(fileName)
      }
  )

  // we assume all dates are in our time zone
  private val format = {
    val dateFormat = "yyyy-[MM][M]-[dd][d]"
    val timeFormat = " HH:mm[:ss]"
    val zoneFormat = "VV"
    val pattern1 = s"[$dateFormat$timeFormat$zoneFormat]"
    val pattern2 = s"[$dateFormat$zoneFormat]"
    val pattern = s"$pattern1$pattern2"
    new java.time.format.DateTimeFormatterBuilder()
      .appendPattern(pattern)
      .parseDefaulting(java.time.temporal.ChronoField.HOUR_OF_DAY, 0)
      .parseDefaulting(java.time.temporal.ChronoField.MINUTE_OF_HOUR, 0)
      .parseDefaulting(java.time.temporal.ChronoField.SECOND_OF_MINUTE, 0)
      .toFormatter()
  }

  private def parseDate(str: String): LocalDateTime =
    LocalDateTime.parse(str+LocalDateTime.myZoneId, format)

  private def exchangesReader(fileName: String) = new CSVSortedOperationReader(fileName) {
    override val linesToSkip = 1

    override def lineScanner(line: String) =
      SeparatedScanner(line, "[,]+")

    override def readLine(line: String, scLn: Scanner): CSVReader.Result[Operation] = {
      val date = parseDate(scLn.next())
      val fromAmount = scLn.nextDouble("soldAmount")
      val fromCurrency = scLn.next("soldCurrency")
      val toAmount = scLn.nextDouble("boughtAmount")
      val toCurrency = scLn.next("boughtCurrency")
      val fee = scLn.nextDouble("Fee")
      val feeCurrency = scLn.next("feeCurrency")
      val exchangerName = scLn.next("Exchanger")
      val desc = scLn.next("Description")

      val exchange =
        Exchange(
          date = date
          , id = ""
          , fromAmount = fromAmount, fromCurrency = Currency.normalize(fromCurrency)
          , toAmount = toAmount, toCurrency = Currency.normalize(toCurrency)
          , fees = List(FeePair(fee, Currency.normalize(feeCurrency)))
          , exchanger = Exchanger.parse(exchangerName)
          , description = desc
        )
      return CSVReader.Ok(exchange)
    }
  }

  private def gainsLossesReader(fileName: String) = new CSVSortedOperationReader(fileName) {
    override val linesToSkip = 1

    override def lineScanner(line: String) =
      SeparatedScanner(line, "[,]+")

    override def readLine(line: String, scLn: Scanner): CSVReader.Result[Operation] = {
      val date = parseDate(scLn.next())
      val amount = scLn.nextDouble("Amount")
      val currency = scLn.next("Currency")
      val fee = scLn.nextDouble("Fee")
      val feeCurrency = scLn.next("feeCurrency")
      val exchangerName = scLn.next("Exchanger")
      val desc = scLn.next("Description")

      val op =
        if(amount>0) {
          Gain(
              date = date
            , id = ""
            , amount = amount
            , currency = currency
            , exchanger = Exchanger.parse(exchangerName)
            , description = desc
          )
        } else {
          Loss(
              date = date
            , id = ""
            , amount = amount.abs
            , currency = currency
            , exchanger = Exchanger.parse(exchangerName)
            , description = desc
          )
        }

      if(fee != 0) {
        val f = Fee(
            date = date
          , id = ""
          , amount = fee
          , currency = feeCurrency
          , exchanger = Exchanger.parse(exchangerName)
          , description = desc
        )
        return CSVReader.Ok(List(op, f))
      } else
        return CSVReader.Ok(op)
    }
  }
}



