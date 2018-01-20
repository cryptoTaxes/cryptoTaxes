package taxes.Exchanger

import taxes.Util.Parse._
import taxes._

object HitBTC extends Exchanger {
  override val id: String = "HitBTC"

  override val sources = Seq(
    new UserFolderSource[Operation]("hitbtc") {
      def fileSource(fileName : String) = operationsReader(fileName)
    }
  )

  private def operationsReader(fileName : String) = new CSVSortedOperationReader(fileName) {
    override val hasHeader: Boolean = true

    override def lineScanner(line: String): Scanner =
      QuotedScanner(line, '\"', ',')

    override def readLine(line: String, scLn: Scanner): CSVReader.Result[Operation] = {
      val date = Date.fromString(scLn.next(), "yyyy-MM-dd hh:mm:ss")
      val instrument = scLn.next()
      val tradeID = scLn.next()
      val orderID = scLn.next()
      val side = scLn.next()
      val quantity = scLn.nextDouble()
      val price = scLn.nextDouble()
      val volume = scLn.nextDouble()
      val fee = scLn.nextDouble()
      val rebate = scLn.nextDouble()
      val total = scLn.nextDouble()

      val desc = HitBTC + " " + tradeID + "/" + orderID

      val (market1,market2) = Parse.split(instrument,"/")
      val isSell = side == "sell"

      // market1 is usually BTC
      if(isSell) {
        val exchange = Exchange(
          date = date
          , id = tradeID + "/" + orderID
          , fromAmount = quantity, fromMarket = Market.normalize(market1)
          , toAmount = total, toMarket = Market.normalize(market2)
          , fee = fee, feeMarket = Market.normalize(market2)
          , exchanger = HitBTC
          , description = desc
        )

        return CSVReader.Ok(exchange)
      } else
        return CSVReader.Warning("%s. Read file. Reading this transaction is not currently supported: %s.".format(id, line))
    }
  }
}
