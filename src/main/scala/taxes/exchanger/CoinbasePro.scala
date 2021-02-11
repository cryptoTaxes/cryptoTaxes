package taxes.exchanger

import taxes._
import taxes.date._
import taxes.io.FileSystem
import taxes.util.parse._


object CoinbasePro extends Exchanger {
  override val id: String = "CoinbasePro"

  override val sources = Seq(
    new FilteredUserInputFolderSource[Operation]("coinbasepro", ".csv") {
      def fileSource(fileName: String) = operationsReader(fileName)
    },
    new FilteredUserInputFolderSource[Operation]("coinbasepro/transactionHistories", ".csv") {
      def fileSource(fileName: String) = transactionHistoriesReader(fileName)
    }/*,
    new FilteredUserInputFolderSource[Operation]("gdax/dw", ".txt") {
      def fileSource(fileName: String)= new FileSource[Operation](fileName) {
        override def read(): Seq[Operation] =
          readDepositsWithdrawals(fileName)
      }
    }*/
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
            , exchanger = CoinbasePro
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
            , exchanger = CoinbasePro
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
      val address = scLn.nextMapOrElse("Address", Some(_), None)
      val relevant = (currency==Currency.euro && transferPaymentMethod.nonEmpty) || _txHash.nonEmpty

      if(relevant) {
        val txHash =
          if(currency==Currency.ethereum || currency==Currency.ethereumClassic)
            s"0x${_txHash}"
          else
            _txHash

        def addLine(rt: RichText, str: String): RichText =
          if(str.isEmpty)
            rt
          else
            rt+RichText.nl+RichText(str)

        if(amount > 0) {
          val deposit = Deposit(
            date = date
            , id = coinbaseId
            , amount = amount
            , currency = currency
            , exchanger = CoinbasePro
            , address = if(currency==Currency.euro) None else address
            , txid = if(currency==Currency.euro) None else Some(txHash)
            , description = addLine(RichText(coinbaseId), transferPaymentMethod)
          )
          return CSVReader.Ok(deposit)
        } else {
          val withdrawal = Withdrawal(
            date = date
            , id = coinbaseId
            , amount = amount.abs
            , currency = currency
            , exchanger = CoinbasePro
            , address = if(currency==Currency.euro) None else Some(to)
            , txid = if(currency==Currency.euro) None else Some(txHash)
            , description = addLine(RichText(coinbaseId), transferPaymentMethod)
          )
          return CSVReader.Ok(withdrawal)
        }
      } else
          return CSVReader.Ignore
    }
  }

  /*
  private def readDepositsWithdrawals(fileName: String): Seq[Operation] = {
    def parseDate(str: String) =
      LocalDateTime.parse(str, "MMM dd, yyyy - [h][hh]:mm:ss a z")

    def isFIAT(currency: Currency): Boolean =
      Seq(Currency.euro,Currency.usd).contains(currency)

    FileSystem.withSource(fileName) { src =>
      val lines = src.getLines().filterNot(taxes.util.parse.Parse.isComment)
      val operations = ListBuffer[Operation]()
      while (lines.hasNext) {
        val date = parseDate(lines.next())
        println(date)
        val currency = Currency.normalize(lines.next())
        val amount = Parse.asDouble(lines.next())
        val asEuros = lines.next()
        if (isFIAT(currency)) {
          val ln = lines.next()
          val prefix1 = "Coinbase Account ID:"
          val prefix2 = "coinbase_payout_at:"
          if(ln.startsWith(prefix1)) {
            // internal tx
            val accountId = Parse.removePrefix(ln, prefix1).get
            val transactionId = Parse.removePrefix(lines.next(), "Coinbase Transaction ID").get
            val withdrawal = Withdrawal(
              date = date
              , id = transactionId
              , amount = amount.abs
              , currency = currency
              , exchanger = GDAX
              , address = Some(accountId)
              , txid = Some(transactionId)
              , description = RichText("Internal transaction")
            )
            operations += withdrawal
          } else if (ln.startsWith(prefix2)) {
            val payoutAt = Parse.removePrefix(ln, prefix2).get
            val accountId = Parse.removePrefix(lines.next(), "Coinbase Account ID:").get
            val withdrawalId = Parse.removePrefix(lines.next(), "coinbase_withdrawal_id:").get
            val transactionId = Parse.removePrefix(lines.next(), "Coinbase Transaction ID:").get
            val paymentMethodId = Parse.removePrefix(lines.next(), "coinbase_payment_method_id:").get
            val paymentMethodType = Parse.removePrefix(lines.next(), "coinbase_payment_method_type:").get
            val withdrawal = Withdrawal(
              date = date
              , id = transactionId
              , amount = amount.abs
              , currency = currency
              , exchanger = GDAX
              , address = Some(accountId)
              , txid = Some(transactionId)
              , description = RichText(paymentMethodType)
            )
            operations += withdrawal
          } else
            Logger.warning(s"GDAX unknown withdrawal: $ln")
        } else {
          val ln = lines.next()
          val (id, address, _txHash) = {
            if(ln.startsWith("Destination Address:")) {
              val address = Parse.skipUntil(ln, "Destination Address:").get
              val txHash = Parse.skipUntil(lines.next(), "Transaction Hash:").get
              val ignore = lines.next()
              (s"$address $txHash", address, txHash)
            } else {
              val address = Parse.skipUntil(ln, "sent_to_address:").get
              val ignore1 = lines.next()
              val ignore2 = lines.next()
              val id = Parse.skipUntil(lines.next(), "Coinbase Transaction ID:").get
              val txHash = Parse.skipUntil(lines.next(), "Transaction Hash:").get
              (id, address, txHash)

            }
          }
          val txHash = if(currency == Currency.ethereum) s"0x${_txHash}" else _txHash
          val withdrawal = Withdrawal(
            date = date
            , id = s"$address $txHash"
            , amount = amount.abs
            , currency = currency
            , exchanger = GDAX
            , address = Some(address)
            , txid = Some(txHash)
            , description = RichText(RichText.util.transaction(currency, txHash, address))
          )
          operations += withdrawal
        }
      }
      return operations.toList
    }
  }
   */
}
