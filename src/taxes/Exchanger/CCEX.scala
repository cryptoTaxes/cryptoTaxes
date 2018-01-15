package taxes.Exchanger

import taxes._

object CCEX extends Exchanger {

  override val id: String = "C-CEX"

  override val folder: String = "c-cex"

  def readFile(fileName : String) : List[Exchange] = {
    val f = new java.io.File(fileName)
    val sc = new java.util.Scanner(f)
    var exchanges = List[Exchange]()
    val header = sc.nextLine()
    while(sc.hasNextLine) {
      val ln = ParseUtils.trimSpaces(sc.nextLine())
      if(ln.nonEmpty) {
        val scLn = new java.util.Scanner(ln).useDelimiter("[ \t]+")
        val token1 = scLn.next()
        val token2 = scLn.next()
        val orderType = scLn.next()

        if(orderType=="Transaction") {
          val date = Date.fromString(token1+" "+token2, "yyyy-MM-dd hh:mm:ss")
          val amount1 = scLn.nextDouble()
          val market1 = scLn.next()
          val amount2 = scLn.nextDouble()
          val market2 = scLn.next()
          scLn.close()

          val feePercent = 0.2

          val exchange =
            Exchange(
              date = date
              , id = ""
              , fromAmount = amount2, fromMarket = Market.normalize(market2)
              , toAmount = amount1, toMarket = Market.normalize(market1)
              , fee = // state fee in BTC
                if (market1=="BTC")
                  amount1 * feePercent / 100
                else
                  amount2 / (1 + 100/feePercent)

              , feeMarket = Market.bitcoin
              , exchanger = CCEX
              , description = id
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

