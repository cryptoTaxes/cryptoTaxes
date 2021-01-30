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
    , new UserInputFolderSource[Operation]("general/networkfees", ".csv") {
      def fileSource(fileName: String) = networkFeesReader(fileName)
    }
    , new UserInputFolderSource[Operation]("general/ledger", ".csv") {
      def fileSource(fileName: String) = ledgerReader(fileName)
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
          , description = RichText(desc)
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
      val desc = RichText(scLn.next("Description"))

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

  private def networkFeesReader(fileName: String) = new CSVSortedOperationReader(fileName) {
    override val linesToSkip = 1

    override def lineScanner(line: String) =
      SeparatedScanner(line, "[,]+")

    override def readLine(line: String, scLn: Scanner): CSVReader.Result[Operation] = {
      val date = parseDate(scLn.next("Date"))
      val amount = scLn.nextDouble("Amount")
      val currency = scLn.next("Currency")
      val hash = scLn.next("Hash")
      val desc = scLn.next("Description")

      val richDesc = RichText(s"$desc fee ${RichText.transaction(currency, hash)}")

      val fee =
        if(Config.config.fundingFees)
          Fee(
            date = date
            , id = hash
            , amount = amount
            , currency = currency
            , exchanger = General("Network fee")
            , description = richDesc
          )
        else
          NonTaxableFee(
            date = date
            , id = hash
            , amount = amount
            , currency = currency
            , exchanger = General("Network fee")
            , description = richDesc
          )
      return CSVReader.Ok(fee)
    }
  }

  private def ledgerReader(fileName: String) = new CSVSortedOperationReader(fileName) {
    override val linesToSkip = 1

    override def lineScanner(line: String) =
      SeparatedScanner(line, "[,]")

    override def readLine(line: String, scLn: Scanner): CSVReader.Result[Operation] = {
      val date = LocalDateTime.parse(scLn.next("Operation Date"), "yyyy-MM-dd'T'HH:mm:ss.SSSX")
      val currency = scLn.next("Currency ticker")
      val opType = scLn.next("Operation type")
      val amount = scLn.nextDouble("Operation amount")
      val feeAmount =
        try {
          scLn.nextDouble("Operation fee")
        } catch {
          case _ => 0
        }
      val hash = scLn.next("Operation Hash")
      var desc =
        try {
          scLn.next("Description")
        } catch {
          case _ => ""
        }

      val isOut = opType == "OUT"
      val isFee = opType == "FEES"
      if((isOut || isFee) && feeAmount>0) {
        val placeholder = "*"
        if (desc.contains(placeholder)) {
          desc =
            if(isOut)
              desc.replace(placeholder, Format.asCurrency(amount - feeAmount, currency))
            else
              desc.replace(placeholder, currency)
          if (desc.last != ' ')
            desc += " "
        }
        val richDesc = RichText(s"$desc fee ${RichText.transaction(currency, hash)}")

        val fee =
          if (Config.config.fundingFees)
            Fee(
              date = date
              , id = hash
              , amount = feeAmount
              , currency = currency
              , exchanger = General("Network fee")
              , description = richDesc
            )
          else
            NonTaxableFee(
              date = date
              , id = hash
              , amount = feeAmount
              , currency = currency
              , exchanger = General("Network fee")
              , description = richDesc
            )
        return CSVReader.Ok(fee)
      } else
        return CSVReader.Ignore

    }
  }
}



