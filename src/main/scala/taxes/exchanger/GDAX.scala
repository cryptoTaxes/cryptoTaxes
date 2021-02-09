package taxes.exchanger

import taxes._
import taxes.date._
import taxes.io.FileSystem
import taxes.util.parse
import taxes.util.parse._

object GDAX extends Exchanger {
  override val id: String = "GDAX"

  override val sources = Seq(
    new FilteredUserInputFolderSource[Operation]("gdax", ".csv") {
      def fileSource(fileName: String) = operationsReader(fileName)
    },
    new FilteredUserInputFolderSource[Operation]("gdax/transactionHistories", ".csv") {
      def fileSource(fileName: String) = transactionHistoriesReader(fileName)
    }
  )

  private def operationsReader(fileName: String) = new CSVSortedOperationReader(fileName) {
    override val linesToSkip = 1

    private lazy val provider = AssociativeSeparatedScannerProvider(skippedLines(0), "[,]")
    override def lineScanner(line: String) =
      provider.scannerFor(line)

    override def readLine(line: String, scLn: Scanner): CSVReader.Result[Operation] = {
      val tradeID = scLn.next("trade id")
      val product = scLn.next("product")
      val side = scLn.next("side")
      val createdAt = scLn.next("created at")
      val size = scLn.nextDouble("size")
      val sizeUnit = scLn.next("size unit")
      val fee = scLn.nextDouble("fee")
      val total = scLn.nextDouble("total")
      val priceFeeTotalUnit = scLn.next("price/fee/total unit")

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

  private def transactionHistoriesReader(fileName: String) = new CSVSortedOperationReader(fileName) {
    override val linesToSkip = 4

    private lazy val provider = AssociativeQuotedScannerProvider(skippedLines(linesToSkip-1), '\"', ',')
    override def lineScanner(line: String): Scanner =
      provider.scannerFor(line)

    override def readLine(line: String, scLn: Scanner): CSVReader.Result[Operation] = {
      val date = LocalDateTime.parse(scLn.next("Timestamp"), "yyyy-MM-dd HH:mm:ss Z")
      val amount = scLn.nextDouble("Amount")
      val currency = Currency.normalize(scLn.next("Currency"))
      val to = scLn.next("To")

      val transferPaymentMethod = scLn.next("Transfer Payment Method")

      val coinbaseId = scLn.next("Coinbase ID")

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
              s"Deposit ${RichText.small(coinbaseId)}${RichText.nl}${RichText.small(transferPaymentMethod.take(40))}..."
            else
              s"Deposit ${RichText.small(coinbaseId)}${RichText.nl}${RichText.util.transaction(currency, txHash)}"

          val deposit = Deposit(
            date = date
            , id = coinbaseId
            , amount = amount
            , currency = currency
            , exchanger = GDAX
            , address = None
            , txid = Some(if(currency==Currency.euro) transferPaymentMethod else txHash)
            , description = RichText(desc)
          )
          return CSVReader.Ok(deposit)
        } else {
          val desc =
            if(currency==Currency.euro)
              s"Withdrawal ${RichText.small(coinbaseId)}${RichText.nl}${RichText.small(transferPaymentMethod.take(40))}..."
            else
              s"Withdrawal ${RichText.small(coinbaseId)}${RichText.nl}${RichText.util.transaction(currency, txHash, to)}"

          val withdrawal = Withdrawal(
            date = date
            , id = coinbaseId
            , amount = amount.abs
            , currency = currency
            , exchanger = GDAX
            , address = if(currency==Currency.euro) None else Some(to)
            , txid = Some(if(currency==Currency.euro) transferPaymentMethod else txHash)
            , description = RichText(desc)
          )
          return CSVReader.Ok(withdrawal)
        }
      } else
          return CSVReader.Ignore
    }
  }
}
