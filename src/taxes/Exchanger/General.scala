package taxes.Exchanger

import taxes.{Date, Exchange, Market, ParseUtils}

object General extends Exchanger {
  override val id: String = "General"

  override val folder: String = "general"

  def readFile(fileName: String): List[Exchange] = {
    val f = new java.io.File(fileName)
    val sc = new java.util.Scanner(f)
    var exchanges = List[Exchange]()
    val header = sc.nextLine()
    while (sc.hasNextLine) {
      val ln = ParseUtils.trimSpaces(sc.nextLine())
      if (ln.nonEmpty) {
        val scLn = new java.util.Scanner(ln).useDelimiter("[,]+")
        val date = Date.fromString(scLn.next(), "yyyy-MM-dd")
        val amount1 = scLn.nextDouble()
        val market1 = scLn.next()
        val amount2 = scLn.nextDouble()
        val market2 = scLn.next()
        val fee = scLn.nextDouble()
        val feeMarket = scLn.next
        val desc = scLn.next()
        scLn.close()

        val exchange =
          Exchange(
            date = date
            , id = ""
            , fromAmount = amount1, fromMarket = Market.normalize(market1)
            , toAmount = amount2, toMarket = Market.normalize(market2)
            , fee = fee
            , feeMarket = Market.normalize(feeMarket)
            , exchanger = General
            , description = id+ " "+desc
          )

        exchanges ::= exchange
      }
    }
    sc.close()
    return exchanges.sortBy(_.date)
  }
}

