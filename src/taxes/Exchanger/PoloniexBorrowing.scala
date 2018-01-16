package taxes.Exchanger

import taxes._

object PoloniexBorrowing extends Exchanger {
  override val id: String = "Poloniex"

  override val folder: String = "poloniex/borrowing"

  def readFile(fileName : String) : List[Operation] = {
    val f = new java.io.File(fileName)
    val sc = new java.util.Scanner(f)
    var operations = List[Operation]()
    val header = sc.nextLine()
    while (sc.hasNextLine) {
      val ln = ParseUtils.trimSpaces(sc.nextLine())
      if (ln.nonEmpty) {
        val scLn = new java.util.Scanner(ln).useDelimiter("[,%]")
        val currency = scLn.next()
        val rate = scLn.nextDouble()
        val amount = scLn.nextDouble()
        val duration = scLn.nextDouble()
        val totalFee = scLn.nextDouble()
        val open = Date.fromString(scLn.next() + " +0000", "yyyy-MM-dd HH:mm:ss Z") // Poloniex time is 1 hour behind here
        val close = Date.fromString(scLn.next() + " +0000", "yyyy-MM-dd HH:mm:ss Z") // Poloniex time is 1 hour behind here
        scLn.close()

        val desc = id + " Borrowing fees"

        val fee = Fee(
          date = close
          , id = desc
          , amount = totalFee
          , market = Market.normalize(currency)
          , exchanger = Poloniex
          , description = desc
        )
        operations ::= fee
      }
    }
    sc.close()
    return operations.sortBy(_.id).sortBy(_.date)
  }
}

