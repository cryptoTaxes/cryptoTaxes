package taxes.exchanger

import taxes._
import taxes.date._
import taxes.io.FileSystem
import taxes.util.parse._


object BitcoinCom extends Exchanger {
  override val id: String = "Bitcoin.com"

  override val sources = Seq(
    new FilteredUserInputFolderSource[Operation]("bitcoin.com", ".csv") {
      def fileSource(fileName: String) = operationsReader(fileName)
    },
    new FilteredUserInputFolderSource[Operation]("bitcoin.com/transactions", ".csv") {
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

    private lazy val provider = AssociativeSeparatedScannerProvider(skippedLines(0), "[,]")
    override def lineScanner(line: String) =
      provider.scannerFor(line)

    private lazy val dateKey = provider.keys.find(_.startsWith("Date")).get

    // Bitcoin.com stores time offset used for dates in csv header line. We take it from there
    private lazy val dateOffset = getDateOffset(dateKey)

    override def readLine(line: String, scLn: Scanner): CSVReader.Result[Operation] = {
      val date = LocalDateTime.parse(scLn.next(dateKey) + dateOffset, "yyyy-MM-dd [H][HH]:mm:ssX")
      val instrument = scLn.next("Instrument")
      val tradeID = scLn.next("Trade ID")
      val orderID = scLn.next("Order ID")
      val side = scLn.next("Side")
      val quantity = scLn.nextDouble("Quantity")
      val price = scLn.nextDouble("Price")
      val fee = scLn.nextDouble("Fee")
      val rebate = scLn.nextDouble("Rebate")

      val desc = RichText(s"Order: $orderID/$tradeID")

      val (m1, m2) = Parse.split(instrument, "/")
      val baseCurrency = Currency.normalize(m1)
      val quoteCurrency = Currency.normalize(m2)

      // quoteCurrency is usually BTC, USDT
      if (side == "sell") {
        val exchange = Exchange(
          date = date
          , id = s"$orderID/$tradeID"
          , fromAmount = quantity, fromCurrency = baseCurrency
          , toAmount = quantity*price - fee + rebate, toCurrency = quoteCurrency //todo check rebate
          , fees = List(FeePair(fee, quoteCurrency))
          , exchanger = BitcoinCom
          , description = desc
        )
        return CSVReader.Ok(exchange)
      } else if(side == "buy") {
        val exchange = Exchange(
          date = date
          , id = s"$orderID/$tradeID"
          , fromAmount = quantity*price - rebate, fromCurrency = quoteCurrency //todo check rebate
          , toAmount = quantity, toCurrency = baseCurrency
          , fees = List(FeePair(fee, quoteCurrency))
          , exchanger = BitcoinCom
          , description = desc
        )
        return CSVReader.Ok(exchange)
      } else
        return CSVReader.Warning(s"$id. Read file ${FileSystem.pathFromData(fileName)}: Reading this transaction is not currently supported: $line.")
    }
  }

  private def paymentsReader(fileName: String) = new CSVSortedOperationReader(fileName) {
    override val linesToSkip = 1

    private lazy val provider = AssociativeSeparatedScannerProvider(skippedLines(0), "[,]")
    override def lineScanner(line: String) =
      provider.scannerFor(line)

    private lazy val dateKey = provider.keys.find(_.startsWith("Date")).get

    // Bitcoin.com stores time offset used for dates in csv header line. We take it from there
    private lazy val dateOffset = getDateOffset(dateKey)

    override def readLine(line: String, scLn: Scanner): CSVReader.Result[Operation] = {
      val date = LocalDateTime.parse(scLn.next(dateKey)+dateOffset, "yyyy-MM-dd [H][HH]:mm:ssX")
      val operationId = scLn.next("Operation id")
      val what = scLn.next("Type")
      val amount = scLn.nextDouble("Amount")
      val txHash = scLn.next("Transaction hash")
      val currency = Currency.normalize(scLn.next("Currency"))

      if(what=="Deposit") {
        val deposit = Deposit(
          date = date
          , id = operationId
          , amount = amount
          , currency = currency
          , exchanger = BitcoinCom
          , address = None
          , txid = Some(txHash)
          , description = RichText(operationId)
        )
        return CSVReader.Ok(deposit)
      } else if(what=="Withdraw" || what=="") { // seems there's a bug a what for withdrawals are empty
        val withdraw = Withdrawal(
          date = date
          , id = operationId
          , amount = amount.abs
          , currency = currency
          , exchanger = BitcoinCom
          , address = None
          , txid = Some(txHash)
          , description = RichText(operationId)
        )
        // todo Withdrawal fees are not currently implemented. They could be
        // calculated using main account balance prior to withdrawal,
        // withdrawn amount and main account new balance after
        return CSVReader.Ok(withdraw)
      } else
        return CSVReader.Warning(s"$id. Read file ${FileSystem.pathFromData(fileName)}: Reading this payment is not currently supported: $line.")
    }
  }
}
