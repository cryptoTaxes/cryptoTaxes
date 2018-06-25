package taxes.Exchanger

import taxes.Market.Market
import taxes.Util.Parse._
import taxes._

object Binance extends Exchanger {
  override val id: String = "Binance"

  override val sources = Seq(
    new UserFolderSource[Operation]("binance", ".csv") {
      def fileSource(fileName : String) = operationsReader(fileName)
    }
  )

  private def operationsReader(fileName : String) = new CSVSortedOperationReader(fileName) {
    override val linesToSkip = 1

    override def lineScanner(line: String): Scanner =
      QuotedScanner(line, '\"', ',')

    private val baseMarkets = List[Market]("BNB", "BTC", "ETH", "USDT")

    override def readLine(line: String, scLn: Scanner): CSVReader.Result[Operation] = {
      val date = Date.fromString(scLn.next("Date"), "yyyy-MM-dd hh:mm:ss")
      val pair = scLn.next("Market")

      baseMarkets.find(pair.endsWith) match {
        case None =>
          return CSVReader.Warning("%s. Read file %s: Reading this transaction is not currently supported: %s. Base pair unknown.".format(id, Paths.pathFromData(fileName), line))
        case Some(suffix) =>

          val market1 = Market.normalize(pair.take(pair.length - suffix.length))
          val market2 = Market.normalize(suffix)

          val orderType = scLn.next("Type")
          val price = scLn.nextDouble("Price")
          val amount = scLn.nextDouble("Amount")
          val total = scLn.nextDouble("Total")
          val feeAmount = scLn.nextDouble("Fee")
          val feeCoin = Market.normalize(scLn.next("Fee Coin"))

          val desc = ""

          // Fee is 0.1% but can get deduced if BNB is used for paying fees.
          // It's applied over toMarket

          // If feeCoin is BNB you get `amount' but you additionally pay a BNB fee
          // Else feeCoin is toMarket and you really get `amount' - `feeAmount'

          if (orderType == "BUY") {
            val toMarket = market1

            if(feeCoin == toMarket) {
              val exchange = Exchange(
                date = date
                , id = ""
                , fromAmount = total, fromMarket = market2
                , toAmount = amount - feeAmount, toMarket = market1
                , fee = feeAmount, feeMarket = feeCoin
                , exchanger = Binance
                , description = desc
              )

              return CSVReader.Ok(exchange)
            } else if(feeCoin == Market.normalize("BNB")) {
              val exchange = Exchange(
                date = date
                , id = ""
                , fromAmount = total, fromMarket = market2
                , toAmount = amount, toMarket = market1
                , fee = 0, feeMarket = market1
                , exchanger = Binance
                , description = desc
              )

              val fee = Fee(
                date = date
                , id = ""
                , amount = feeAmount
                , market = feeCoin
                , exchanger = Binance
                , description = desc
              )

              return CSVReader.Ok(List(exchange, fee))
            } else
              return CSVReader.Warning("%s. Read file %s: Reading this transaction is not currently supported: %s.".format(id, Paths.pathFromData(fileName), line))
          } else if (orderType == "SELL") {
            val toMarket = market2

            if(feeCoin == toMarket) {
              val exchange = Exchange(
                date = date
                , id = ""
                , fromAmount = amount, fromMarket = market1
                , toAmount = total - feeAmount, toMarket = market2
                , fee = feeAmount, feeMarket = feeCoin
                , exchanger = Binance
                , description = desc
              )

              return CSVReader.Ok(exchange)
            } else if(feeCoin == Market.normalize("BNB")) {
              val exchange = Exchange(
                date = date
                , id = ""
                , fromAmount = amount, fromMarket = market1
                , toAmount = total, toMarket = market2
                , fee = 0, feeMarket = market2
                , exchanger = Binance
                , description = desc
              )

              val fee = Fee(
                date = date
                , id = ""
                , amount = feeAmount
                , market = feeCoin
                , exchanger = Binance
                , description = desc
              )

              return CSVReader.Ok(List(exchange, fee))
            } else
              return CSVReader.Warning("%s. Read file %s: Reading this transaction is not currently supported: %s.".format(id, Paths.pathFromData(fileName), line))

          } else
            return CSVReader.Warning("%s. Read file %s: Reading this transaction is not currently supported: %s.".format(id, Paths.pathFromData(fileName), line))
      }
    }
  }
}
