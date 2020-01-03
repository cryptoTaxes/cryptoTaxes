package taxes.exchanger

import taxes._
import taxes.date._
import taxes.io.FileSystem
import taxes.util.parse._


object HitBTC extends Exchanger {
  override val id: String = "HitBTC"

  override val sources = Seq(
    new UserInputFolderSource[Operation]("hitbtc", ".csv") {
      def fileSource(fileName: String) = operationsReader(fileName)
    },
    new UserInputFolderSource[Operation]("hitbtc/payments", ".csv") {
      def fileSource(fileName: String) = paymentsReader(fileName)
    }
  )

  private def operationsReader(fileName: String) = new CSVSortedOperationReader(fileName) {
    override val linesToSkip = 1

    override def lineScanner(line: String): Scanner =
      QuotedScanner(line, '\"', ',')

    // HitBTC shows time offset used for dates in csv header line. We take it from there
    private lazy val offset = skippedLines(0).dropWhile(_ != '(').tail.takeWhile(_ != ')')

    override def readLine(line: String, scLn: Scanner): CSVReader.Result[Operation] = {
      val date = LocalDateTime.parse(scLn.next("Date")+offset, "yyyy-MM-dd [H][HH]:mm:ssX")
      val instrument = scLn.next("Instrument")
      val tradeID = scLn.next("Trade ID")
      val orderID = scLn.next("Order ID")
      val side = scLn.next("Side")
      val quantity = scLn.nextDouble("Quantity")
      val price = scLn.nextDouble("Price")
      val volume = scLn.nextDouble("Volume")
      val fee = scLn.nextDouble("Fee")
      val rebate = scLn.nextDouble("Rebate")
      val total = scLn.nextDouble("Total")

      val desc = RichText(s"Order: $tradeID / $orderID")

      val (m1,m2) = Parse.split(instrument,"/")
      val baseCurrency = Currency.normalize(m1)
      val quoteCurrency = Currency.normalize(m2)
      val isSell = side == "sell"

      // quoteCurrency is usually BTC
      if(isSell) {
        val exchange = Exchange(
          date = date
          , id = tradeID + "/" + orderID
          , fromAmount = quantity, fromCurrency = baseCurrency
          , toAmount = volume - fee + rebate, toCurrency = quoteCurrency
          , fees = List(FeePair(fee, quoteCurrency))
          , exchanger = HitBTC
          , description = desc
        )

        return CSVReader.Ok(exchange)
      } else //toDo add support for buy orders
        return CSVReader.Warning(s"$id. Read file ${FileSystem.pathFromData(fileName)}: Reading this transaction is not currently supported: $line.")
    }
  }

  private def paymentsReader(fileName: String) = new CSVSortedOperationReader(fileName) {
    override val linesToSkip = 1

    override def lineScanner(line: String): Scanner =
      QuotedScanner(line, '\"', ',')

    // HitBTC shows time offset used for dates in csv header line. We take it from there
    private lazy val offset = skippedLines(0).dropWhile(_ != '(').tail.takeWhile(_ != ')')

    override def readLine(line: String, scLn: Scanner): CSVReader.Result[Operation] = {
      val date = LocalDateTime.parse(scLn.next("Date")+offset, "yyyy-MM-dd [H][HH]:mm:ssX")
      val operationId = scLn.next("Operation id")
      val what = scLn.next("Type")
      val amount = scLn.nextDouble("Amount")
      val txHash = scLn.next("Transaction Hash")
      val mainAccountBalance = scLn.nextDouble("Main account balance")
      val currency = Currency.normalize(scLn.next("Currency"))

      if(what=="Deposit") {
        val desc = RichText(s"Deposit ${RichText.util.transaction(currency, txHash)}")
        val deposit = Deposit(
          date = date
          , id = operationId
          , amount = amount
          , currency = currency
          , exchanger = HitBTC
          , description = desc
        )
        return CSVReader.Ok(deposit)
      } else if(what=="Withdraw") {
        val desc = RichText(s"Withdrawal ${RichText.util.transaction(currency, txHash)}")
        val withdraw = Withdrawal(
          date = date
          , id = operationId
          , amount = amount.abs
          , currency = currency
          , exchanger = HitBTC
          , description = desc
        )
        return CSVReader.Ok(withdraw)
      } else
        return CSVReader.Warning(s"$id. Read file ${FileSystem.pathFromData(fileName)}: Reading this payment is not currently supported: $line.")
    }
  }
}
