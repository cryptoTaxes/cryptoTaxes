package taxes.exchanger

import taxes._
import taxes.date._
import taxes.io.FileSystem
import taxes.util.Logger
import taxes.util.parse._

import scala.collection.mutable.ListBuffer


object Changelly extends Exchanger {
  override val id: String = "Changelly"

  private def split(str: String): (Double, Currency) = {
    val token = str.filter(_ != ',')
    val sc = SeparatedScanner(token, "[ ]")
    val amount = sc.nextDouble("amount")
    val currency = Currency.normalize(sc.next("currency"))
    sc.close()
    return (amount, currency)
  }

  override val sources = Seq(
    new FilteredUserInputFolderSource[Operation]("changelly/2017", ".csv") {
      def fileSource(fileName: String) = operationsReader2017(fileName)
    },
    new FilteredUserInputFolderSource[Operation]("changelly/2017/depositsWithdrawals", ".txt") {
      def fileSource(fileName: String)= new FileSource[Operation](fileName) {
        override def read(): Seq[Operation] =
          readDepositsWithdrawals2017(fileName)
      }
    }, /* Exchanges can be properly obtained from deposits/withdrawals text file
    new UserInputYearFolderSource[Operation]("changelly/2020", ".csv") {
      def fileSource(fileName: String) = operationsReader2020(fileName)
    }, */
    new FilteredUserInputFolderSource[Operation]("changelly/2020/depositsWithdrawals", ".txt") {
      def fileSource(fileName: String)= new FileSource[Operation](fileName) {
        override def read(): Seq[Operation] =
          readDepositsWithdrawals2020(fileName)
      }
    }
  )

  private def operationsReader2017(fileName: String) = new CSVSortedOperationReader(fileName) {
    override val linesToSkip = 1

    override def lineScanner(line: String): Scanner =
      QuotedScanner(line, '\"', ',')

    override def readLine(line: String, scLn: Scanner): CSVReader.Result[Operation] = {
      val status = scLn.next("Status")
      if(status=="finished") {
        val date = LocalDateTime.parseAsUTC(scLn.next("Date"), "dd MMM yyyy, HH:mm:ss")  // Changelly transactions-history.csv file uses UTC time zone
                                                                                         // Note that the detailed transactions shown in the GUI use a different time zone
        val (fromAmount, fromCurrency) = split(scLn.next("Sold"))
        val (totalFee, feeCurrency) = split(scLn.next("Fee"))

        val (token1, token2) = Parse.split(scLn.next("Exchange Rate"), " = ")
        val (rateSold, soldCurrency) = split(token1)
        val (rateReceived, receivedCurrency) = split(token2)
        val exchangeRate = rateReceived / rateSold

        val receiverWallet = scLn.next("Receiver Wallet")
        val (toAmount, toCurrency) = split(scLn.next("Received"))

        if(soldCurrency != fromCurrency || receivedCurrency != toCurrency)
          return CSVReader.Warning(s"$id. Read file ${FileSystem.pathFromData(fileName)}: cannot parse this line: $line as rate is not expressed as $toCurrency/$fromCurrency.")

        val realFee = fromAmount * exchangeRate - toAmount
        val feePercent = realFee * 100 / (fromAmount * exchangeRate)

        val desc = RichText(s"Order: $receiverWallet")

        val exchange = Exchange(
            date = date
            , id = receivedCurrency
            , fromAmount = fromAmount, fromCurrency = fromCurrency
            , toAmount = toAmount, toCurrency = toCurrency
            , fees = List(FeePair(realFee, feeCurrency))
            , exchanger = Changelly
            , description = desc
        )
        return CSVReader.Ok(List(exchange))
      } else
        return CSVReader.Warning(s"$id. Read file ${FileSystem.pathFromData(fileName)}: cannot parse this line: $line.")
    }
  }

  private def readDepositsWithdrawals2017(fileName: String): Seq[Operation] = {
    def parseDate(str: String) =
      LocalDateTime.parseAsMyZoneId(str, "dd MMM yyyy, HH:mm:ss")

    FileSystem.withSource(fileName) { src =>
      val lines = src.getLines().filterNot(taxes.util.parse.Parse.isComment)
      val operations = ListBuffer[Operation]()
      while(lines.hasNext) {
        val Array(_,id,_,inHash,_,inAmountLine,_,inDate,_,feeLine,_,_,_,receiver,_,outHash,_,outAmountLine,_,outDate) = lines.take(20).toArray

        val (inAmount, inCurrency) = split(inAmountLine)
        val (feeAmount, feeCurrency) = split(feeLine)
        val (outAmount, outCurrency) = split(outAmountLine)

        val deposit = Deposit(
          parseDate(inDate)
          , id = id
          , amount = inAmount
          , currency = inCurrency
          , exchanger = Changelly
          , description = RichText(s"Deposit ${RichText.util.transaction(inCurrency, inHash)}")
        )

        val withdrawal = Withdrawal(
          parseDate(outDate)
          , id = id
          , amount = outAmount
          , currency = outCurrency
          , exchanger = Changelly
          , description = RichText(s"Withdrawal ${RichText.util.transaction(outCurrency, outHash, receiver)}")
        )
        operations += deposit
        operations += withdrawal
      }
      return operations.toList
    }
  }

  private def operationsReader2020(fileName: String) = new CSVSortedOperationReader(fileName) {
    override val linesToSkip = 1

    override def lineScanner(line: String): Scanner =
      QuotedScanner(line, '\"', ',')

    override def readLine(line: String, scLn: Scanner): CSVReader.Result[Operation] = {
      val fromCurrency = Currency.normalize(scLn.next("Currency from"))
      val toCurrency = Currency.normalize(scLn.next("Currency to"))
      val status = scLn.next("Status")

      if(status=="finished") {
        val date = LocalDateTime.parseAsUTC(scLn.next("Date"), "dd MMM yyyy, HH:mm:ss")  // Changelly transactions-history.csv file uses UTC time zone
        // Note that the detailed transactions shown in the GUI use a different time zone

        val exchangeAmount = scLn.nextDouble("Exchange amount")
        val feeRate = scLn.nextDouble("Total fee")
        val exchangeRate = scLn.nextDouble("Exchange rate")
        val receiver = scLn.next("Receiver")
        val toAmount = scLn.nextDouble("Amount received")

        val fee = exchangeAmount * feeRate / 100 // expressed in fromCurrency
        val fromAmount = exchangeAmount - fee

        val desc = RichText(s"Order: $receiver")

        val exchange = Exchange(
            date = date
            , id = toCurrency
            , fromAmount = fromAmount, fromCurrency = fromCurrency
            , toAmount = toAmount, toCurrency = toCurrency
            , fees = List(FeePair(fee, fromCurrency))
            , exchanger = Changelly
            , description = desc
        )
        return CSVReader.Ok(List(exchange))
      } else
        return CSVReader.Warning(s"$id. Read file ${FileSystem.pathFromData(fileName)}: cannot parse this line: $line.")
    }
  }

  private def readDepositsWithdrawals2020(fileName: String): Seq[Operation] = {
    def parseDate(str: String) =
      LocalDateTime.parseAsMyZoneId(str, "dd MMM yyyy, HH:mm:ss")

    FileSystem.withSource(fileName) { src =>
      val lines = src.getLines().filterNot(taxes.util.parse.Parse.isComment)
      val operations = ListBuffer[Operation]()
      while(lines.hasNext) {
        val array@Array(_,id,_,_,inAmountLine,_,outAmountLine,_,inDate,_,outDate,_,exchRateLine,_,receiver,_,inHash,_,outHash) = lines.take(19).toArray

        val (inAmount, inCurrency) = split(inAmountLine)
        val (outAmount, outCurrency) = split(outAmountLine)

        val (token1, token2) = Parse.split(exchRateLine, " = ")
        val (rateSold, rateSoldCurrency) = split(token1)
        val (rateReceived, rateReceivedCurrency) = split(token2)

        if(inCurrency != rateSoldCurrency || outCurrency != rateReceivedCurrency)
          Logger.fatal(s"$id. Read file ${FileSystem.pathFromData(fileName)}: cannot parse these lines: ${array.mkString("\n")} as rate is not expressed as $outCurrency/$inCurrency.")

        val toAmount = outAmount
        val fromAmount = toAmount * rateSold / rateReceived
        val fee = inAmount - fromAmount // expressed in inCurrency

        val depositDate = parseDate(inDate)

        val deposit = Deposit(
          date = depositDate
          , id = id
          , amount = inAmount
          , currency = inCurrency
          , exchanger = Changelly
          , description = RichText(s"Deposit ${RichText.util.transaction(inCurrency, inHash)}")
        )

        val exchange = Exchange(
            date = depositDate.plusNanos(1) // to keep proper order for deposit/exchange/withdrawal
            , id = id
            , fromAmount = fromAmount, fromCurrency = inCurrency
            , toAmount = toAmount, toCurrency = outCurrency
            , fees = List(FeePair(fee, inCurrency))
            , exchanger = Changelly
            , description = RichText(s"Order: $id")
        )

        val withdrawal = Withdrawal(
          parseDate(outDate)
          , id = id
          , amount = outAmount
          , currency = outCurrency
          , exchanger = Changelly
          , description = RichText(s"Withdrawal ${RichText.util.transaction(outCurrency, outHash, receiver)}")
        )
        operations += deposit
        operations += exchange
        operations += withdrawal
      }
      return operations.toList
    }
  }
}



