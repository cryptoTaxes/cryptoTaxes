package taxes.Exchanger

import taxes.Market.Market
import taxes.Util.Parse.{CSVSortedOperationReader, QuotedScanner, Scanner}
import taxes._

object Changelly extends Exchanger {

  override val id: String = "Changelly"

  private def split(str : String) : (Double, Market) = {
    val token = str.filter(_ != ',')
    val sc = new java.util.Scanner(token).useDelimiter("[ ]")
    val amount = sc.nextDouble()
    val market = sc.next()
    sc.close()
    return (amount, market)
  }

  override val sources = Seq(
    new UserFolderSource[Operation]("changelly") {
      def fileSource(fileName : String) = operationsReader(fileName)
    }
  )

  private def operationsReader(fileName : String) = new CSVSortedOperationReader(fileName) {
    override val hasHeader: Boolean = true

    override def lineScanner(line: String): Scanner =
      QuotedScanner(line, '\"', ',')

    override def readLine(line: String, scLn: Scanner): Either[String, Operation] = {
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
        return Right(exchange)
      } else
        return Left("%s. Read file: cannot parse this line: %s.".format(id, line))
    }
  }
}


