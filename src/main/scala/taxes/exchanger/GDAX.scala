package taxes.exchanger

import taxes._
import taxes.date._
import taxes.io.FileSystem
import taxes.util.parse
import taxes.util.parse._

object GDAX extends Exchanger {
  override val id: String = "GDAX"

  override val sources = Seq(
    new UserInputFolderSource[Operation]("gdax", ".csv") {
      def fileSource(fileName: String) = operationsReader(fileName)
    }, /*
    new UserInputFolderSource[Operation]("gdax/accounts", ".csv") {
      def fileSource(fileName: String) = depositsWithdrawalsReader(fileName)
    },*/
    new UserInputFolderSource[Operation]("gdax/transactionHistories", ".csv") {
      def fileSource(fileName: String) = transactionHistoriesReader(fileName)
    }
  )

  private def operationsReader(fileName: String) = new CSVSortedOperationReader(fileName) {
    override val linesToSkip = 1

    override def lineScanner(line: String): Scanner =
      SeparatedScanner(line, "[,]")

    override def readLine(line: String, scLn: Scanner): CSVReader.Result[Operation] = {
      val tradeID = scLn.next("Trade ID")
      val product = scLn.next("Product")
      val side = scLn.next("Side")
      val createdAt = scLn.next("Created At")
      val size = scLn.nextDouble("Size")
      val sizeUnit = scLn.next("Size Unit")
      val price = scLn.nextDouble("Price")
      val fee = scLn.nextDouble("Fee")
      val total = scLn.nextDouble("Total")
      val priceFeeTotalUnit = scLn.next("Price/Total Unit")

      val desc = RichText(s"Order: $tradeID")
      val date = LocalDateTime.parse(createdAt, "yyyy-MM-dd'T'HH:mm:ss.SSSX") // GDAX includes a zone-offset 'Z' at the end

      val (_baseCurrency, _quoteCurrency) = Parse.split(product, "-")
      val baseCurrency = Currency.normalize(_baseCurrency)
      val quoteCurrency = Currency.normalize(_quoteCurrency)

      val sizeCurrency = Currency.normalize(sizeUnit)
      val priceFeeTotalCurrency = Currency.normalize(priceFeeTotalUnit)

      if(sizeCurrency != baseCurrency)
        return CSVReader.Warning(s"$id. Read file ${FileSystem.pathFromData(fileName)}: Reading this transaction is not currently supported: $line as sizeCurrency($sizeCurrency) and quoteCurrency($quoteCurrency) are different.")

      if(side == "SELL") {
        val exchange =
          Exchange(
            date = date
            , id = tradeID
            , fromAmount = size, fromCurrency = baseCurrency
            , toAmount = total, toCurrency = quoteCurrency
            , fees = List(FeePair(fee, priceFeeTotalCurrency))
            , exchanger = GDAX
            , description = desc
          )
        return CSVReader.Ok(exchange)
      } else if(side == "BUY") {
        val exchange =
          Exchange(
            date = date
            , id = tradeID
            , fromAmount = total.abs, fromCurrency = quoteCurrency
            , toAmount = size, toCurrency = baseCurrency
            , fees = List(FeePair(fee, priceFeeTotalCurrency))
            , exchanger = GDAX
            , description = desc
          )
        return CSVReader.Ok(exchange)
      } else
        return CSVReader.Warning(s"$id. Read file ${FileSystem.pathFromData(fileName)}: Reading this transaction is not currently supported: $line.")
    }
  }

  private def depositsWithdrawalsReader(fileName: String) = new CSVSortedOperationReader(fileName) {
    override val linesToSkip = 1

    override def lineScanner(line: String): Scanner =
      SeparatedScanner(line, "[,]")

    override def readLine(line: String, scLn: Scanner): CSVReader.Result[Operation] = {
      val profile = scLn.next("profile")
      val what = scLn.next("type")
      val time = LocalDateTime.parse(scLn.next("time"), "yyyy-MM-dd'T'HH:mm:ss.SSSX") // GDAX includes a zone-offset 'Z' at the end
      val amount = scLn.nextDouble("amount")
      val balance = scLn.nextDouble("balance")
      val unit = Currency.normalize(scLn.next("amount/balance unit"))
      val id = scLn.next("transfer id,trade id,order id")

      if(what=="deposit") {
        val deposit = Deposit(
          date = time
          , id = id
          , amount = amount
          , currency = unit
          , exchanger = GDAX
          , description = RichText(s"Deposit $id")
        )
        return CSVReader.Ok(deposit)
      } else if(what=="withdrawal") {
        val withdrawal = Withdrawal(
          date = time
          , id = id
          , amount = amount.abs
          , currency = unit
          , exchanger = GDAX
          , description = RichText(s"Withdrawal $id")
        )
        return CSVReader.Ok(withdrawal)
      } else
        return CSVReader.Warning(s"$id. Read file ${FileSystem.pathFromData(fileName)}: Reading this transaction is not currently supported: $line.")
    }
  }

  private def transactionHistoriesReader(fileName: String) = new CSVSortedOperationReader(fileName) {
    override val linesToSkip = 4

    override def lineScanner(line: String): Scanner =
      parse.QuotedScanner(line, '\"', ',')

    override def readLine(line: String, scLn: Scanner): CSVReader.Result[Operation] = {
      val date = LocalDateTime.parse(scLn.next("Timestamp"), "yyyy-MM-dd HH:mm:ss Z")
      val balance = scLn.nextDouble("Balance")
      val amount = scLn.nextDouble("Amount")
      val currency = Currency.normalize(scLn.next("Currency"))
      val to = scLn.next("To")

      val _0 = scLn.next("Notes")
      val _1 = scLn.next("Instantly Exchanged")
      val _2 = scLn.next("Transfer Total")
      val _3 = scLn.next("Transfer Total Currency")
      val _4 = scLn.next("Transfer Fee")
      val _5 = scLn.next("Transfer Fee Currency")
      val transferPaymentMethod = scLn.next("Transfer Payment Method")
      val _7 = scLn.next("Transfer ID")
      val _8 = scLn.next("Order Price")
      val _9 = scLn.next("Order Currency")
      val _10 = scLn.next("Order Total")
      val _11 = scLn.next("Order Tracking Code")
      val _12 = scLn.next("Order Custom Parameter")
      val _13 = scLn.next("Order Paid Out")
      val _14 = scLn.next("Recurring Payment ID")

      val id = scLn.next("Coinbase ID")

      val _txHash = scLn.next("Transaction Hash")
      val relevant = (currency==Currency.euro && transferPaymentMethod.nonEmpty) || _txHash.nonEmpty

      if(relevant) {
        val txHash =
          if(currency==Currency.ethereum || currency==Currency.ethereumClassic)
            s"0x${_txHash}"
          else
            _txHash

        if(amount > 0) {
          val desc =
            if(currency==Currency.euro)
              s"Deposit ${transferPaymentMethod.take(20)}..."
            else
              s"Deposit ${RichText.util.transaction(currency, txHash)}"

          val deposit = Deposit(
            date = date
            , id = id
            , amount = amount
            , currency = currency
            , exchanger = GDAX
            , description = RichText(desc)
          )
          return CSVReader.Ok(deposit)
        } else {
          val desc =
            if(currency==Currency.euro)
              s"Withdrawal ${transferPaymentMethod.take(20)}..."
            else
              s"Withdrawal ${RichText.util.transaction(currency, txHash, to)}"

          val withdrawal = Withdrawal(
            date = date
            , id = id
            , amount = amount.abs
            , currency = currency
            , exchanger = GDAX
            , description = RichText(desc)
          )
          return CSVReader.Ok(withdrawal)
        }
      } else
          return CSVReader.Ignore
    }
  }
}
