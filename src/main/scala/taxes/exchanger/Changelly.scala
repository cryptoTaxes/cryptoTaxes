package taxes.exchanger

import taxes._
import taxes.date._
import taxes.io.FileSystem
import taxes.util.parse._

import scala.collection.mutable.ListBuffer


object Changelly extends Exchanger {

  override val id: String = "Changelly"

  private def split(str: String): (Double, Currency) = {
    val token = str.filter(_ != ',')
    val sc = SeparatedScanner(token, "[ ]")
    val amount = sc.nextDouble()
    val currency = sc.next()
    sc.close()
    return (amount, currency)
  }

  override val sources = Seq(
    new UserInputFolderSource[Operation]("changelly", ".csv") {
      def fileSource(fileName: String) = operationsReader(fileName)
    },
    new UserInputFolderSource[Operation]("changelly/depositsWithdrawals", ".txt") {
      def fileSource(fileName: String)= new FileSource[Operation](fileName) {
        override def read(): Seq[Operation] =
          readDepositsWithdrawals(fileName)
      }
    }
  )

  private def operationsReader(fileName: String) = new CSVSortedOperationReader(fileName) {
    override val linesToSkip = 1

    override def lineScanner(line: String): Scanner =
      QuotedScanner(line, '\"', ',')

    override def readLine(line: String, scLn: Scanner): CSVReader.Result[Operation] = {
      val status = scLn.next("Status")
      if(status=="finished") {
        val date = LocalDateTime.parseAsUTC(scLn.next("Date"), "dd MMM yyyy, HH:mm:ss")  // Changelly transactions-history.csv file uses UTC time zone
                                                                                         // Note that the detailed transactions shown in the GUI use a different time zone
        val (fromAmount, soldCurrency) = split(scLn.next("Sold"))
        val (totalFee, feeCurrency) = split(scLn.next("Fee"))

        val (token1, token2) = Parse.split(scLn.next("Exchange Rate"), " = ")
        val (rateSold, soldCurrency1) = split(token1)
        val (rateReceived, receivedCurrency1) = split(token2)
        val exchangeRate = rateReceived / rateSold

        val receiverWallet = scLn.next("Receiver Wallet")
        val (toAmount, receivedCurrency) = split(scLn.next("Received"))

        if(soldCurrency1 != soldCurrency || receivedCurrency1 != receivedCurrency)
          CSVReader.Warning(s"$id. Read file ${FileSystem.pathFromData(fileName)}: cannot parse this line: $line as rate is not expressed as receivedCurrency/soldCurrency.")

        val realFee = fromAmount * exchangeRate - toAmount
        val feePercent = realFee * 100 / (fromAmount * exchangeRate)

        val desc = RichText(s"Order: $receiverWallet")

        val fromCurrency = Currency.normalize(soldCurrency)
        val toCurrency = Currency.normalize(receivedCurrency)

        /*
        val deposit = Deposit(
          date = date
          , id = receivedCurrency
          , amount = fromAmount
          , currency = fromCurrency
          , exchanger = Changelly
          , description = RichText("Deposit")
        )
        */
        val exchange =
          Exchange(
            date = date
            , id = receivedCurrency
            , fromAmount = fromAmount, fromCurrency = fromCurrency
            , toAmount = toAmount, toCurrency = toCurrency
            , fees = List(FeePair(realFee, Currency.normalize(feeCurrency)))
            , exchanger = Changelly
            , description = desc
          )
        /*
        val withdrawal = Withdrawal(
          date = date
          , id = receivedCurrency
          , amount = toAmount
          , currency = toCurrency
          , exchanger = Changelly
          , description = RichText(s"Withdrawal ${RichText.util.address(toCurrency, receiverWallet)}")
        )
        */
        return CSVReader.Ok(List(exchange))
      } else
        return CSVReader.Warning(s"$id. Read file ${FileSystem.pathFromData(fileName)}: cannot parse this line: $line.")
    }
  }

  private def readDepositsWithdrawals(fileName: String): Seq[Operation] = {
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
          , id
          , inAmount
          , inCurrency
          , Changelly
          , RichText(s"Deposit ${RichText.util.transaction(inCurrency, inHash)}")
        )

        /*
        val exchange =
          Exchange(
            date = parseDate(inDate).minusSeconds(120)
            , id = id
            , fromAmount = inAmount, fromCurrency = inCurrency
            , toAmount = outAmount, toCurrency = outCurrency
            , fees = List(FeePair(feeAmount, Currency.normalize(feeCurrency)))
            , exchanger = Changelly
            , description = RichText(s"Order: $id")
          )
        */

        val withdrawal = Withdrawal(
          parseDate(outDate)
          , id
          , outAmount
          , outCurrency
          , Changelly
          , RichText(s"Withdrawal ${RichText.util.transaction(outCurrency, outHash, receiver)}")
        )
        operations += deposit
        //operations += exchange
        operations += withdrawal
      }
      return operations.toList
    }
  }
}



