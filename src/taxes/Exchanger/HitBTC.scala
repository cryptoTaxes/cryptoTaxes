package taxes.Exchanger

import taxes.Util.Parse._
import taxes._

object HitBTC extends Exchanger {
  override val id: String = "HitBTC"

  override val sources = Seq(
    new UserFolderSource[Operation]("hitbtc", ".csv") {
      def fileSource(fileName : String) = operationsReader(fileName)
    }
  )

  private def operationsReader(fileName : String) = new CSVSortedOperationReader(fileName) {
    override val hasHeader: Boolean = true

    override def lineScanner(line: String): Scanner =
      QuotedScanner(line, '\"', ',')

    override def readLine(line: String, scLn: Scanner): CSVReader.Result[Operation] = {
      val date = Date.fromString(scLn.next("Date"), "yyyy-MM-dd hh:mm:ss")
      val instrument = scLn.next("Instrument")
      val tradeID = scLn.next("Trade ID")
      val orderID = scLn.next("Order ID")
      val side = scLn.next("Side")
      val quantity = scLn.nextDouble("Quantity")
      val price = scLn.nextDouble("Price")
      val volume = scLn.nextDouble("Volume")
      val fee = scLn.nextDouble("Fee")
      val rebate = scLn.nextDouble("Rebate")
      val total = scLn.nextDouble("Total")

      val desc = "Order: " + tradeID + "/" + orderID

      val (market1,market2) = Parse.split(instrument,"/")
      val isSell = side == "sell"

      // market2 is usually BTC
      if(isSell) {
        val exchange = Exchange(
          date = date
          , id = tradeID + "/" + orderID
          , fromAmount = quantity, fromMarket = Market.normalize(market1)
          , toAmount = volume - fee + rebate, toMarket = Market.normalize(market2)
          , fee = fee, feeMarket = Market.normalize(market2)
          , exchanger = HitBTC
          , description = desc
        )

        return CSVReader.Ok(exchange)
      } else
        return CSVReader.Warning("%s. Read file %s: Reading this transaction is not currently supported: %s.".format(id, Paths.pathFromData(fileName), line))
    }
  }
}
