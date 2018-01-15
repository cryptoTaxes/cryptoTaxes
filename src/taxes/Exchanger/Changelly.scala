package taxes.Exchanger

import taxes.Market.Market
import taxes._

object Changelly extends Exchanger {

  override val id: String = "Changelly"

  override val folder: String = "changelly"

  private def split(str : String) : (Double, Market) = {
    val token = str.filter(_ != ',')
    val sc = new java.util.Scanner(token).useDelimiter("[ ]")
    val amount = sc.nextDouble()
    val market = sc.next()
    sc.close()
    return (amount, market)
  }

  def readFile(fileName : String) : List[Exchange] = {
    val f = new java.io.File(fileName)
    val sc = new java.util.Scanner(f)
    var exchanges = List[Exchange]()
    val header = sc.nextLine()
    while(sc.hasNextLine) {
      val ln = ParseUtils.trimSpaces(sc.nextLine())
      if(ln.nonEmpty) {
        val scLn = QuotedScanner(ln, '\"', ',')

        val status = scLn.next()
        if(status=="finished") {
          val date = Date.fromString(scLn.next()+" +0000", "dd MMM yyyy, HH:mm:ss Z")

          val (amount, soldMarket) = split(scLn.next())
          val (totalFee, feeMarket) = split(scLn.next())

          val token1 = scLn.next()
          val (token2, token3) = token1.span(_ != '=')
          val (exchangeRate, _) = split(token3.drop(2))
          val receiverWallet = scLn.next()

          val (amountReceived, receivedMarket) = split(scLn.next())

          val realFee = amount * exchangeRate - amountReceived
          val feePercent = realFee * 100 / (amount * exchangeRate)

          val desc = id + " " + receiverWallet

          val exchange =
            Exchange(
              date = date
              , id = receivedMarket
              , fromAmount = amount, fromMarket = Market.normalize(soldMarket)
              , toAmount = amountReceived, toMarket = Market.normalize(receivedMarket)
              , fee = realFee, feeMarket = Market.normalize(feeMarket)
              , exchanger = Changelly
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


