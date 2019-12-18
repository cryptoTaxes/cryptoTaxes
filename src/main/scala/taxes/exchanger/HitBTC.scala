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

      val desc = "Order: " + tradeID + "/" + orderID

      val (m1,m2) = Parse.split(instrument,"/")
      val baseMarket = Market.normalize(m1)
      val quoteMarket = Market.normalize(m2)
      val isShort = side == "sell"

      // quoteMarket is usually BTC
      if(isShort) {
        val exchange = Exchange(
          date = date
          , id = tradeID + "/" + orderID
          , fromAmount = quantity, fromMarket = baseMarket
          , toAmount = volume - fee + rebate, toMarket = quoteMarket
          , fees = List(FeePair(fee, quoteMarket))
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
      val currency = Market.normalize(scLn.next("Currency"))

      if(what=="Deposit") {
        val desc = "Deposit " + txHash
        val deposit = Deposit(
          date = date
          , id = operationId
          , amount = amount
          , market = currency
          , exchanger = HitBTC
          , description = desc
        )
        return CSVReader.Ok(deposit)
      } else if(what=="Withdraw") {
        val desc = "Withdrawal " + txHash
        val withdraw = Withdrawal(
          date = date
          , id = operationId
          , amount = amount.abs
          , market = currency
          , exchanger = HitBTC
          , description = desc
        )
        return CSVReader.Ok(withdraw)
      } else
        return CSVReader.Warning(s"$id. Read file ${FileSystem.pathFromData(fileName)}: Reading this payment is not currently supported: $line.")
    }
  }
}
