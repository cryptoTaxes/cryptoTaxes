package taxes.exchanger

import taxes._
import taxes.date._
import taxes.io.FileSystem
import taxes.util.parse._


object Binance extends Exchanger {
  override val id: String = "Binance"

  override val sources = Seq(
    new UserInputFolderSource[Operation]("binance", ".csv") {
      def fileSource(fileName : String) = operationsReader(fileName)
    }
  )

  private def operationsReader(fileName : String) = new CSVSortedOperationReader(fileName) {
    override val linesToSkip = 1

    override def lineScanner(line: String): Scanner =
      QuotedScanner(line, '\"', ',')

    private val baseMarkets = List[Market]("BNB", "BTC", "ETH", "USDT")

    override def readLine(line: String, scLn: Scanner): CSVReader.Result[Operation] = {
      val date = LocalDateTime.parseAsUTC(scLn.next("Date"), "yyyy-MM-dd HH:mm:ss") // Binance trade history xlsx file uses UTC time zone
      val pair = scLn.next("Market")

      baseMarkets.find(pair.endsWith) match {
        case None =>
          return CSVReader.Warning(s"$id. Read file ${FileSystem.pathFromData(fileName)}: Reading this transaction is not currently supported: $line. Base pair unknown.")
        case Some(suffix) =>

          val baseMarket = Market.normalize(pair.take(pair.length - suffix.length))
          val quoteMarket = Market.normalize(suffix)

          val orderType = scLn.next("Type")
          val price = scLn.nextDouble("Price")
          val amount = scLn.nextDouble("Amount")
          val total = scLn.nextDouble("Total")
          val feeAmount = scLn.nextDouble("Fee")
          val feeCoin = Market.normalize(scLn.next("Fee Coin"))

          val desc = ""

          // Fee is 0.1% but can get deduced if BNB is used for paying fees.
          // It's applied over toMarket

          // If feeCoin is toMarket (even if it is BNB) you really get
          // `amount' - `feeAmount'
          // Else you get `amount' but you additionally pay a BNB fee

          if (orderType == "BUY") {
            // toDo check this further
            // This has to be the first case as if you're buying BNB and you pay
            // your fee with BNB, you get `amount` - `feeAmount'
            if(feeCoin == baseMarket) {
              val exchange = Exchange(
                date = date
                , id = ""
                , fromAmount = total, fromMarket = quoteMarket
                , toAmount = amount - feeAmount, toMarket = baseMarket
                , fees = List(FeePair(feeAmount, feeCoin))
                , exchanger = Binance
                , description = desc
              )
              return CSVReader.Ok(exchange)
            } else if(feeCoin == Market.normalize("BNB")) {
              val exchange = Exchange(
                date = date
                , id = ""
                , fromAmount = total, fromMarket = quoteMarket
                , toAmount = amount, toMarket = baseMarket
                , fees = List(FeePair(feeAmount, feeCoin))
                , exchanger = Binance
                , description = desc
              )
              return CSVReader.Ok(exchange)
            } else
              return CSVReader.Warning(s"$id. Read file ${FileSystem.pathFromData(fileName)}: Reading this transaction is not currently supported: $line.")
          } else if (orderType == "SELL") {
            if(feeCoin == quoteMarket) {
              val exchange = Exchange(
                date = date
                , id = ""
                , fromAmount = amount, fromMarket = baseMarket
                , toAmount = total - feeAmount, toMarket = quoteMarket // toDo the fact that we don't get total but total - feeAmount whe selling needs to be confirmed
                , fees = List(FeePair(feeAmount, feeCoin))
                , exchanger = Binance
                , description = desc
              )
              return CSVReader.Ok(exchange)
            } else if(feeCoin == Market.normalize("BNB")) {
              val exchange = Exchange(
                date = date
                , id = ""
                , fromAmount = amount, fromMarket = baseMarket
                , toAmount = total, toMarket = quoteMarket
                , fees = List(FeePair(feeAmount, feeCoin))
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
}
