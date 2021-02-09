package taxes.exchanger

import taxes._
import taxes.date._
import taxes.io.FileSystem
import taxes.util.parse._


object HitBTC extends Exchanger {
  override val id: String = "HitBTC"

  override val sources = Seq(
    new FilteredUserInputFolderSource[Operation]("hitbtc", ".csv") {
      def fileSource(fileName: String) = operationsReader(fileName)
    },
    new FilteredUserInputFolderSource[Operation]("hitbtc/payments", ".csv") {
      def fileSource(fileName: String) = paymentsReader(fileName)
    }
  )

  private def getDateOffset(dateKey: String): String = {
    val inParenthesis = Parse.trim(dateKey, !"()".contains(_))
    val offset  = Parse.unquote(inParenthesis, "(", ")").get
    if(offset=="UTC") "+00" else offset
  }

  private def operationsReader(fileName: String) = new CSVSortedOperationReader(fileName) {
    override val linesToSkip = 1

    private lazy val provider = AssociativeQuotedScannerProvider(skippedLines(0), '\"', ',')
    override def lineScanner(line: String): Scanner =
      provider.scannerFor(line)

    private lazy val dateKey = provider.keys.find(_.startsWith("Date")).get

    // HitBTC stores time offset used for dates in csv header line. We take it from there
    private lazy val dateOffset = getDateOffset(dateKey)

    override def readLine(line: String, scLn: Scanner): CSVReader.Result[Operation] = {
      val date = LocalDateTime.parse(scLn.next(dateKey)+dateOffset, "yyyy-MM-dd [H][HH]:mm:ssX")
      val instrument = scLn.next("Instrument")
      val tradeID = scLn.next("Trade ID")
      val orderID = scLn.next("Order ID")
      val side = scLn.next("Side")
      val quantity = scLn.nextDouble("Quantity")
      val volume = scLn.nextDouble("Volume")
      val fee = scLn.nextDouble("Fee")
      val rebate = scLn.nextDouble("Rebate")

      val desc = RichText(s"Order: $orderID/$tradeID")

      val (m1,m2) = Parse.split(instrument,"/")
      val baseCurrency = Currency.normalize(m1)
      val quoteCurrency = Currency.normalize(m2)
      val isSell = side == "sell"

      // quoteCurrency is usually BTC
      if(isSell) {
        val exchange = Exchange(
          date = date
          , id = s"$orderID/$tradeID"
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

    private lazy val provider = AssociativeQuotedScannerProvider(skippedLines(0), '\"', ',')
    override def lineScanner(line: String): Scanner =
      provider.scannerFor(line)

    private lazy val dateKey = provider.keys.find(_.startsWith("Date")).get

    // HitBTC stores time offset used for dates in csv header line. We take it from there
    private lazy val dateOffset = getDateOffset(dateKey)

    override def readLine(line: String, scLn: Scanner): CSVReader.Result[Operation] = {
      val date = LocalDateTime.parse(scLn.next(dateKey)+dateOffset, "yyyy-MM-dd [H][HH]:mm:ssX")
      val operationId = scLn.next("Operation id")
      val what = scLn.next("Type")
      val amount = scLn.nextDouble("Amount")
      val txHash = scLn.next("Transaction Hash")
      val currency = Currency.normalize(scLn.next("")) // yep!, currency has an empty string as header

      if(what=="Deposit") {
        val desc = RichText(s"Deposit ${RichText.small(operationId)}${RichText.nl}${RichText.util.transaction(currency, txHash)}")
        val deposit = Deposit(
          date = date
          , id = operationId
          , amount = amount
          , currency = currency
          , exchanger = HitBTC
          , address = None
          , txid = Some(txHash)
          , description = desc
        )
        return CSVReader.Ok(deposit)
      } else if(what=="Withdraw") {
        val desc = RichText(s"Withdrawal ${RichText.small(operationId)}${RichText.nl}${RichText.util.transaction(currency, txHash)}")
        val withdraw = Withdrawal(
          date = date
          , id = operationId
          , amount = amount.abs
          , currency = currency
          , exchanger = HitBTC
          , address = None
          , txid = Some(txHash)
          , description = desc
        )
        return CSVReader.Ok(withdraw)
      } else
        return CSVReader.Warning(s"$id. Read file ${FileSystem.pathFromData(fileName)}: Reading this payment is not currently supported: $line.")
    }
  }
}
