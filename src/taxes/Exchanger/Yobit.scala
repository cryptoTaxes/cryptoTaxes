package taxes.Exchanger

import taxes.{Date, Exchange, Market, ParseUtils}

object Yobit extends Exchanger {
  override val id: String = "Yobit"

  override val folder: String = "yobit"

  def readFile(fileName : String) : List[Exchange] = {
    val f = new java.io.File(fileName)
    val sc = new java.util.Scanner(f)
    var exchanges = List[Exchange]()
    val header = sc.nextLine()
    while(sc.hasNextLine) {
      val ln = ParseUtils.trimSpaces(sc.nextLine())
      if(ln.nonEmpty) {
        val scLn = new java.util.Scanner(ln).useDelimiter("[ \t]")

        val date1 = scLn.next()
        val date2 = scLn.next()

        val date = Date.fromString(date1+" "+date2, "yyyy-MM-dd hh:mm:ss")

        val pair = scLn.next()
        val orderType = scLn.next()
        val price = scLn.nextDouble()
        val amount = scLn.nextDouble()
        val total = scLn.nextDouble()
        scLn.close()

        val desc = id

        val (market1,market2) = ParseUtils.split(pair,"/")
        val isSell = orderType == "SELL"

        // market1 is usually BTC
        val exchange =
          if(isSell)
            Exchange(
              date = date
              , id = ""
              , fromAmount = amount, fromMarket = Market.normalize(market1)
              , toAmount = total, toMarket = Market.normalize(market2)
              , fee = 0, feeMarket = Market.normalize(market2)
              , exchanger = Yobit
              , description = desc
            )
          else
            Exchange(
              date = date
              , id = ""
              , fromAmount = total, fromMarket = Market.normalize(market2)
              , toAmount = amount, toMarket = Market.normalize(market1)
              , fee = 0, feeMarket = Market.normalize(market1)
              , exchanger = Yobit
              , description = desc
            )
        exchanges ::= exchange
      }
    }
    sc.close()
    return exchanges.sortBy(_.date)
  }
}
