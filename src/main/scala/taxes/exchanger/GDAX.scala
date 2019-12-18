package taxes.exchanger

import taxes._
import taxes.date._
import taxes.io.FileSystem
import taxes.util.parse._

object GDAX extends Exchanger {
  override val id: String = "GDAX"

  override val sources = Seq(
    new UserInputFolderSource[Operation]("gdax", ".csv") {
      def fileSource(fileName: String) = operationsReader(fileName)
    },
    new UserInputFolderSource[Operation]("gdax/accounts", ".csv") {
      def fileSource(fileName: String) = depositsWithdrawalsReader(fileName)
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

      val desc = "Order: " + tradeID
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
          , description = "Deposit " + id
        )
        return CSVReader.Ok(deposit)
      } else if(what=="withdrawal") {
        val withdrawal = Withdrawal(
          date = time
          , id = id
          , amount = amount.abs
          , currency = unit
          , exchanger = GDAX
          , description = "Withdrawal " + id
        )
        return CSVReader.Ok(withdrawal)
      } else
        return CSVReader.Warning(s"$id. Read file ${FileSystem.pathFromData(fileName)}: Reading this transaction is not currently supported: $line.")
    }
  }
}
