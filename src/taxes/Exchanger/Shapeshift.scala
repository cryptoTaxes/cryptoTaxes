package taxes.Exchanger

import taxes.{Date, Exchange, Market, ParseUtils}

object Shapeshift extends Exchanger {
  override val id: String = "Shapeshift"

  override val folder: String = "shapeshift"

  def readFile(fileName : String) : List[Exchange] = {
    val f = new java.io.File(fileName)
    val sc = new java.util.Scanner(f)
    var exchanges = List[Exchange]()
    while (sc.hasNextLine) {
      val ln0 = ParseUtils.trimSpaces(sc.nextLine())
      val orderId = ln0.dropWhile(_ != ':').takeWhile(_ != ';')
      val desc = id + " " + orderId

      val ln1 = ParseUtils.trimSpaces(sc.nextLine())
      val market1 = ln1.tail.takeWhile(_ != ';')

      val ln2 = ParseUtils.trimSpaces(sc.nextLine())
      val market2 = ln2.tail.takeWhile(_ != ';')

      val ln3 = ParseUtils.trimSpaces(sc.nextLine())
      val scLn3 = new java.util.Scanner(ln3).useDelimiter("[;]")
      scLn3.next()
      val amount2 = scLn3.next().replace(',','.').toDouble
      scLn3.close()

      val ln4 = ParseUtils.trimSpaces(sc.nextLine())
      val scLn4 = new java.util.Scanner(ln4).useDelimiter("[;]")
      scLn4.next()
      val amount1 = scLn4.next().replace(',','.').toDouble
      scLn4.close()

      val ln5 = ParseUtils.trimSpaces(sc.nextLine())
      val scLn5 = new java.util.Scanner(ln5).useDelimiter("[;]")
      val date = Date.fromString(scLn5.next(), "yyyy-MM-dd hh:mm:ss")
      sc.nextLine()

      val exchange =
        Exchange(
          date = date
          , id = orderId
          , fromAmount = amount1, fromMarket = Market.normalize(market1)
          , toAmount = amount2, toMarket = Market.normalize(market2)
          , fee = 0 // toDo fix me
          , feeMarket = Market.normalize(market1)
          , exchanger = Shapeshift
          , description = desc
        )

      exchanges ::= exchange
    }
    sc.close()
    return exchanges.sortBy(_.date)
  }
}
