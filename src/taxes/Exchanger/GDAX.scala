package taxes.Exchanger

import taxes._

object GDAX extends Exchanger {
  override val id: String = "GDAX"

  override val folder: String = "gdax"

  def readFile(fileName : String) : List[Exchange] = {
    val f = new java.io.File(fileName)
    val sc = new java.util.Scanner(f)
    var exchanges = List[Exchange]()
    val header = sc.nextLine()
    while(sc.hasNextLine) {
      val ln = ParseUtils.trimSpaces(sc.nextLine())
      if(ln.nonEmpty) {
        val scLn = new java.util.Scanner(ln).useDelimiter("[,]")
        val tradeID = scLn.next()
        val product = scLn.next()
        val side = scLn.next()
        val createdAt = scLn.next()
        val size = scLn.nextDouble()
        val sizeUnit = scLn.next()
        val price = scLn.nextDouble()
        val fee = scLn.nextDouble()
        val total = scLn.nextDouble()
        val priceFeeTotalUnit = scLn.next()
        scLn.close()

        if (side == "SELL") {
          val desc = id + " " + tradeID
          val date = Date.fromString(createdAt, "yyyy-MM-dd'T'HH:mm:ss.SSSX")

          val (market1, aux) = product.span(_ != '-')
          val market2 = aux.tail

          exchanges ::=
            Exchange(
              date = date
              , id = tradeID
              , fromAmount = size, fromMarket = Market.normalize(sizeUnit)
              , toAmount = total, toMarket = Market.normalize(priceFeeTotalUnit)
              , fee = fee, feeMarket = Market.normalize(priceFeeTotalUnit)
              , exchanger = GDAX
              , description = desc
            )
        } else
          Logger.warning("%s.readExchanges. Reading this transaction is not currently supported: %s.".format(id,ln))
      }
    }
    sc.close()
    return exchanges.sortBy(_.date)
  }
}
