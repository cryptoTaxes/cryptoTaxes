package taxes.Exchanger

import taxes.Util.Parse.{CSVSortedOperationReader, Parse, QuotedScanner, Scanner}
import taxes._

object HitBTC extends Exchanger {
  override val id: String = "HitBTC"

  override val folder: String = "hitbtc"

  private val operationsReader = new CSVSortedOperationReader {
    override val hasHeader: Boolean = true

    override def lineScanner(line: String): Scanner =
      QuotedScanner(line, '\"', ',')

    override def readLine(line: String, scLn: Scanner): Either[String, Operation] = {
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

      val desc = id + " " + tradeID + "/" + orderID

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

        return Right(exchange)
      } else
        return Left("%s.readExchanges. Reading this transaction is not currently supported: %s.".format(id, line))
    }
  }

  def readFile(fileName : String) : List[Operation] =
    operationsReader.readFile(fileName)
}
