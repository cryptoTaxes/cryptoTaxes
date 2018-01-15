package taxes.Exchanger

import taxes._

object RippleTrade extends Exchanger {
  override val id: String = "XRP Trade"

  override val folder: String = "xrptrade"

  def readFile(fileName : String) : List[Exchange] = {
    val f = new java.io.File(fileName)
    val sc = new java.util.Scanner(f)
    var exchanges = List[Exchange]()

    val desc = id

    while(sc.hasNextLine) {
      val ln = ParseUtils.trimSpaces(sc.nextLine())
      if(ln.nonEmpty) {
        val scLn = new java.util.Scanner(ln).useDelimiter("[,]")

        val date = Date.fromString(scLn.next(), "yyyy-MM-dd hh:mm:ss")
        val what = scLn.next()

        if (what == "Sell") {
          val amount1 = scLn.nextDouble()
          val market1 = scLn.next()
          scLn.next()
          val amount2 = scLn.nextDouble()
          val market2 = scLn.next()
          scLn.next()
          val price = scLn.nextDouble()

          exchanges ::=
            Exchange(
              date = date
              , id = ""
              , fromAmount = amount1, fromMarket = Market.normalize(market1)
              , toAmount = amount2, toMarket = Market.normalize(market2)
              , fee = 0, feeMarket = Market.normalize(market2)
              , exchanger = RippleTrade
              , description = desc
            )
        } else if (what == "Buy") {
          val amount1 = scLn.nextDouble()
          val market1 = scLn.next()
          scLn.next()
          val amount2 = scLn.nextDouble()
          val market2 = scLn.next()
          scLn.next()
          val price = scLn.nextDouble()

          exchanges ::=
            Exchange(
              date = date
              , id = ""
              , fromAmount = amount2, fromMarket = Market.normalize(market2)
              , toAmount = amount1, toMarket = Market.normalize(market1)
              , fee = 0, feeMarket = Market.normalize(market1)
              , exchanger = RippleTrade
              , description = desc
            )
        } else
          Logger.warning("%s.readExchanges. Reading this transaction is not currently supported: %s.".format(id,ln))
        scLn.close()
      }
    }
    sc.close()
    return exchanges.sortBy(_.date)
  }
}
