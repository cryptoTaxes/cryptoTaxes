package taxes.exchanger

import taxes._
import taxes.date._
import taxes.io.FileSystem
import taxes.util.parse._


object Coinbase extends Exchanger {
  override val id: String = "Coinbase"

  override val sources = Seq(
    new UserInputFolderSource[Operation]("coinbase/transfers", ".csv") {
      def fileSource(fileName : String) = operationsReader(fileName)
    }
  )

  private def operationsReader(fileName : String) = new CSVSortedOperationReader(fileName) {
    override val linesToSkip = 3

    override def lineScanner(line: String): Scanner =
      QuotedScanner(line.replace('“', '\"').replace('”', '\"'), '\"', ',')

    lazy val baseMarket = {
      val sc = SeparatedScanner(skippedLines(2), "[,]")
      sc.next()
      sc.next()
      Market.normalize(sc.next())
    }

    override def readLine(line: String, scLn: Scanner): CSVReader.Result[Operation] = {
      val timeStamp = scLn.next("Timestamp")
      val orderType = scLn.next("Type")
      val amount = scLn.nextDouble("Amount")
      val subtotal = scLn.nextDouble("Subtotal")
      val fees = scLn.next("Fees")
      val total = scLn.nextDouble("Total")
      val currency = scLn.next("Currency")
      val price = scLn.nextDouble("Price")
      val paymentMethod = scLn.next("Payment Method")

      val desc = paymentMethod
      val id = desc
      val date = LocalDateTime.parse(timeStamp, "yyyy-MM-dd HH:mm:ss Z")

      val quoteMarket = Market.normalize(currency)

      val feeAmount = total - subtotal

      if (orderType == "Sell") {
        val exchange =
          Exchange(
            date = date
            , id = id
            , fromAmount = amount, fromMarket = baseMarket
            , toAmount = subtotal.abs, toMarket = quoteMarket
            , fees = List(FeePair(feeAmount, quoteMarket)) // todo detached Coinbase doesn't use fee to compute exchange rate so we add fee apart
            , exchanger = Coinbase
            , description = desc
          )
        return CSVReader.Ok(exchange)
      } else if (orderType == "Buy") {
        val exchange =
          Exchange(
            date = date
            , id = id
            , fromAmount = subtotal.abs, fromMarket = quoteMarket
            , toAmount = amount, toMarket = baseMarket
            , fees = List(FeePair(feeAmount, quoteMarket)) // todo detached Coinbase doesn't use fee to compute exchange rate so we add fee apart
            , exchanger = Coinbase
            , description = desc
          )
        return CSVReader.Ok(exchange)
      } else
        return CSVReader.Warning(s"$id. Read file ${FileSystem.pathFromData(fileName)}: Reading this transaction is not currently supported: $line.")
    }
  }
}
