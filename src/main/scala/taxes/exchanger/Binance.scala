package taxes.exchanger

import taxes._
import taxes.date._
import taxes.io.FileSystem
import taxes.util.Logger
import taxes.util.parse._


object Binance extends Exchanger {
  override val id: String = "Binance"

  override val sources = Seq(
    new FilteredUserInputFolderSource[Operation]("binance", ".xlsx") {
      def fileSource(xlsxFileName: String) = operationsReader(xlsxFileName)
    },
    new FilteredUserInputFolderSource[Operation]("binance/deposits", ".xlsx") {
      def fileSource(xlsxFileName: String) = depositsReader(xlsxFileName)
    },
    new FilteredUserInputFolderSource[Operation]("binance/withdrawals", ".xlsx") {
      def fileSource(xlsxFileName: String) = withdrawalsReader(xlsxFileName)
    }
  )

  abstract class BinanceReader(xlsxFileName: String) extends {
    val (path, name, ext) = FileSystem.decompose(xlsxFileName)
    val csvFileName = FileSystem.compose(Seq(path, "generated"), name, ".csv")
  } with CSVReader[Operation](csvFileName) {

    private val csvSeparator = ','

    override def preprocess() = Some { () =>
      Logger.trace(s"Generating $csvFileName file from $xlsxFileName.")
      ExcelReader.XLSXToCSV(xlsxFileName, csvFileName, sep = csvSeparator)
    }

    override val linesToSkip = 1

    private lazy val provider = AssociativeSeparatedScannerProvider(skippedLines(0), s"[$csvSeparator]")
    override def lineScanner(line: String) =
      provider.scannerFor(line)
}

  private def operationsReader(xlsxFileName: String) =
    new BinanceReader(xlsxFileName) {
      private val baseCurrencies = List[Currency]("BNB", "BTC", "ETH", "USDT")

      override def readLine(line: String, scLn: Scanner): CSVReader.Result[Operation] = {
        val date = LocalDateTime.parseAsUTC(scLn.next("Date"), "yyyy-MM-dd HH:mm:ss") // Binance trade history xlsx file uses UTC time zone
        val pair = scLn.next("Market")

        baseCurrencies.find(pair.endsWith) match {
          case None =>
            return CSVReader.Warning(s"$id. Read file ${FileSystem.pathFromData(fileName)}: Reading this transaction is not currently supported: $line. Base pair unknown.")
          case Some(suffix) =>

            val baseCurrency = Currency.normalize(pair.take(pair.length - suffix.length))
            val quoteCurrency = Currency.normalize(suffix)

            val orderType = scLn.next("Type")
            val price = scLn.nextDouble("Price")
            val amount = scLn.nextDouble("Amount")
            val total = scLn.nextDouble("Total")
            val feeAmount = scLn.nextDouble("Fee")
            val feeCurrency = Currency.normalize(scLn.next("Fee Coin"))

            val desc = RichText("")

            // Fee is 0.1% but can get deduced if BNB is used for paying fees.
            // It's applied over toCurrency

            // If feeCurrency is toCurrency (even if it is BNB) you really get
            // `amount' - `feeAmount'
            // Else you get `amount' but you additionally pay a BNB fee

            if(orderType == "BUY") {
              // toDo check this further
              // This has to be the first case as if you're buying BNB and you pay
              // your fee with BNB, you get `amount` - `feeAmount'
              if(feeCurrency == baseCurrency) {
                val exchange = Exchange(
                  date = date
                  , id = ""
                  , fromAmount = total, fromCurrency = quoteCurrency
                  , toAmount = amount - feeAmount, toCurrency = baseCurrency
                  , fees = List(FeePair(feeAmount, feeCurrency))
                  , exchanger = Binance
                  , description = desc
                )
                return CSVReader.Ok(exchange)
              } else if(feeCurrency == Currency.normalize("BNB")) {
                val exchange = Exchange(
                  date = date
                  , id = ""
                  , fromAmount = total, fromCurrency = quoteCurrency
                  , toAmount = amount, toCurrency = baseCurrency
                  , fees = List(FeePair(feeAmount, feeCurrency))
                  , exchanger = Binance
                  , description = desc
                )
                return CSVReader.Ok(exchange)
              } else
                return CSVReader.Warning(s"$id. Read file ${FileSystem.pathFromData(fileName)}: Reading this transaction is not currently supported: $line.")
            } else if(orderType == "SELL") {
              if(feeCurrency == quoteCurrency) {
                val exchange = Exchange(
                  date = date
                  , id = ""
                  , fromAmount = amount, fromCurrency = baseCurrency
                  , toAmount = total - feeAmount, toCurrency = quoteCurrency // toDo the fact that we don't get total but total - feeAmount whe selling needs to be confirmed
                  , fees = List(FeePair(feeAmount, feeCurrency))
                  , exchanger = Binance
                  , description = desc
                )
                return CSVReader.Ok(exchange)
              } else if(feeCurrency == Currency.normalize("BNB")) {
                val exchange = Exchange(
                  date = date
                  , id = ""
                  , fromAmount = amount, fromCurrency = baseCurrency
                  , toAmount = total, toCurrency = quoteCurrency
                  , fees = List(FeePair(feeAmount, feeCurrency))
                  , exchanger = Binance
                  , description = desc
                )
                return CSVReader.Ok(exchange)
              } else
                return CSVReader.Warning(s"$id. Read file ${FileSystem.pathFromData(fileName)}: Reading this transaction is not currently supported: $line.")
            } else
              return CSVReader.Warning(s"$id. Read file ${FileSystem.pathFromData(fileName)}: Reading this transaction is not currently supported: $line.")
        }
      }
    }

  private def depositsReader(xlsxFileName: String) =
    new BinanceReader(xlsxFileName) {
      override def readLine(line: String, scLn: Scanner): CSVReader.Result[Operation] = {
        val date = LocalDateTime.parseAsUTC(scLn.next("Date"), "yyyy-MM-dd HH:mm:ss") // Binance uses UTC time zone
        val currency = Currency.normalize(scLn.next("Coin"))
        val amount = scLn.nextDouble("Amount")
        val address = scLn.next("Address")
        val txid = scLn.next("TXID")
        val completed = scLn.next("Status") == "Completed"

        if(completed) {
          val desc = RichText(s"Deposit ${RichText.util.transaction(currency, txid, address)}")
          val deposit = Deposit(
            date = date
            , id = s"$address $txid"
            , amount = amount
            , currency = currency
            , exchanger = Binance
            , address = Some(address)
            , txid = Some(txid)
            , description = desc
          )
          return CSVReader.Ok(deposit)
        } else
          CSVReader.Warning(s"$id. Read deposit ${FileSystem.pathFromData(fileName)}: This deposit was not completed: $line.")
      }
    }

  private def withdrawalsReader(xlsxFileName: String) =
    new BinanceReader(xlsxFileName) {
      override def readLine(line: String, scLn: Scanner): CSVReader.Result[Operation] = {
        val date = LocalDateTime.parseAsUTC(scLn.next("Date"), "yyyy-MM-dd HH:mm:ss") // Binance uses UTC time zone
        val currency = Currency.normalize(scLn.next("Coin"))
        val amount = scLn.nextDouble("Amount")
        val address = scLn.next("Address")
        val txid = scLn.next("TXID")
        val completed = scLn.next("Status") == "Completed"

        if(completed) {
          val desc =  RichText(s"Withdrawal ${RichText.util.transaction(currency, txid, address)}")
          val withdrawal = Withdrawal(
            date = date
            , id = address
            , amount = amount
            , currency = currency
            , exchanger = Binance
            , address = Some(address)
            , txid = Some(txid)
            , description = desc
          )
          return CSVReader.Ok(withdrawal)
        } else
          CSVReader.Warning(s"$id. Read withdrawal ${FileSystem.pathFromData(fileName)}: This withdrawal was not completed: $line.")
      }
    }
}
