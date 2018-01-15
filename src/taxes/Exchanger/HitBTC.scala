package taxes.Exchanger

import taxes._

object HitBTC extends Exchanger {
  override val id: String = "HitBTC"

  override val folder: String = "hitbtc"

  def readFile(fileName : String) : List[Exchange] = {
    val f = new java.io.File(fileName)
    val sc = new java.util.Scanner(f)
    var exchanges = List[Exchange]()
    val header = sc.nextLine()
    while(sc.hasNextLine) {
      val ln = ParseUtils.trimSpaces(sc.nextLine())
      if(ln.nonEmpty) {
        val scLn = QuotedScanner(ln, '\"', ',')

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

        val (market1,market2) = ParseUtils.split(instrument,"/")
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

          exchanges ::= exchange
        } else
          Logger.warning("%s.readExchanges. Reading this transaction is not currently supported: %s.".format(id,ln))
      }
    }
    sc.close()
    return exchanges.sortBy(_.date)
  }
}
