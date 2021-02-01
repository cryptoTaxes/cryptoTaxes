package taxes.exchanger

import taxes._
import taxes.date._
import taxes.io.FileSystem
import taxes.util.parse._

import scala.collection.mutable.ListBuffer


object QTrade extends Exchanger {
  override val id: String = "QTrade"

  override val sources = Seq(
    new FilteredUserInputFolderSource[Operation]("qtrade", ".csv") {
      def fileSource(fileName: String) = operationsReader(fileName)
    },
    new FilteredUserInputFolderSource[Operation]("qtrade/depositsWithdrawals", ".txt") {
      def fileSource(fileName: String)= new FileSource[Operation](fileName) {
        override def read(): Seq[Operation] =
          readDepositsWithdrawals(fileName)
      }
    }
  )

  private def operationsReader(fileName: String) = new CSVSortedOperationReader(fileName) {
    override val linesToSkip = 1

    lazy val header = {
      // manually remove BOM if present in UTF8 file
      val UTF8_BOM = "\uFEFF"
      val line = skippedLines(0)
      if(line.startsWith(UTF8_BOM)) line.substring(1) else line
    }

    private lazy val provider = AssociativeQuotedScannerProvider(header, '\"', ',')
    override def lineScanner(line: String): Scanner =
      provider.scannerFor(line)

    override def readLine(line: String, scLn: Scanner): CSVReader.Result[Operation] = {
      val orderId = scLn.next("Order ID")
      val orderType = scLn.next("Type")
      val marketCurrency = Currency.normalize(scLn.next("Market Currency"))
      val baseCurrency = Currency.normalize(scLn.next("Base Currency"))
      val tradeId = scLn.next("Trade ID")
      val marketAmount = scLn.nextDouble("Market Amount")
      val baseAmount = scLn.nextDouble("Base Amount")
      val baseFee = scLn.nextDouble("Base Fee")
      val date = LocalDateTime.parse(scLn.next("Creation Date"), "yyyy-MM-dd'T'HH:mm:ss.SSSSSSX") // includes a zone-offset 'Z' at the end

      val id = s"$orderId/$tradeId"
      val desc = RichText(s"Order: $id")
      val isBuy = orderType.startsWith("buy")

      // toDo All exchanges we currently have are 0 fee exchanges. We need trades with non-zero fees to
      // see how they are
      val exchange =
        if(isBuy)
          Exchange(
            date = date
            , id = id
            , fromAmount = baseAmount, fromCurrency = baseCurrency
            , toAmount = marketAmount, toCurrency = marketCurrency
            , fees = List(FeePair(baseFee, baseCurrency))
            , exchanger = QTrade
            , description = desc
          )
        else
          Exchange(
            date = date
            , id = id
            , fromAmount = marketAmount, fromCurrency = marketCurrency
            , toAmount = baseAmount, toCurrency = baseCurrency
            , fees = List(FeePair(baseFee, baseCurrency))
            , exchanger = QTrade
            , description = desc
          )
      return CSVReader.Ok(exchange)
    }
  }

  private def readDepositsWithdrawals(fileName: String): Seq[Operation] = {
    FileSystem.withSource(fileName) { src =>
      val lines = src.getLines().filterNot(taxes.util.parse.Parse.isComment)
      val operations = ListBuffer[Operation]()
      while(lines.hasNext) {
        val array@Array(dateLine,whatLine,amountLine,currencyLine,address,hash) = lines.take(6).map(Parse.trimSpaces).toArray

        val date = LocalDateTime.parseAsMyZoneId(dateLine, "yyyy/MM/dd HH:mm")
        val isWithdrawal = whatLine == "WITHDRAW"
        val amount = Parse.asDouble(amountLine).abs
        val currency = Currency.normalize(currencyLine)

        if(isWithdrawal) {
          val feeAmount = Parse.sepBy(lines.next(), " ").map(Parse.asDouble).sum
          val fee =
            if(Config.config.fundingFees)
              Fee(
                date = date.plusNanos(1) // to put it in order after withdrawal
                , id = hash
                , amount = feeAmount
                , currency = currency
                , exchanger = QTrade
                , description = RichText(s"QTrade withdrawal of ${Format.asCurrency(amount-feeAmount, currency)} fee")
              )
            else
              NonTaxableFee(
                date = date.plusNanos(1) // to put it in order after withdrawal
                , id = hash
                , amount = feeAmount
                , currency = currency
                , exchanger = QTrade
                , description = RichText(s"QTrade withdrawal of ${Format.asCurrency(amount-feeAmount, currency)}non taxable fee")
              )

          val withdrawal = Withdrawal(
            date = date
            , id = hash
            , amount = amount - feeAmount
            , currency = currency
            , exchanger = QTrade
            , description = RichText(s"Withdrawal ${RichText.util.transaction(currency, hash, address)}")
          )
          operations += withdrawal
          operations += fee
        } else {
          val deposit = Deposit(
            date = date
            , id = hash
            , amount = amount
            , currency = currency
            , exchanger = QTrade
            , description = RichText(s"Deposit ${RichText.util.transaction(currency, hash, address)}")
          )
          operations += deposit
        }
      }
      return operations.toList
    }
  }
}
