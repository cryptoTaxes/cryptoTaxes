package taxes.Exchanger

import taxes.Market.Market
import taxes.Util.Parse.{CSVReader, CSVSortedOperationReader, QuotedScanner, Scanner}
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
    new UserFolderSource[Operation]("changelly", ".csv") {
      def fileSource(fileName : String) = operationsReader(fileName)
    }
  )

  private def operationsReader(fileName : String) = new CSVSortedOperationReader(fileName) {
    override val linesToSkip = 1

    override def lineScanner(line: String): Scanner =
      QuotedScanner(line, '\"', ',')

    override def readLine(line: String, scLn: Scanner): CSVReader.Result[Operation] = {
      val status = scLn.next("Status")
      if(status=="finished") {
        val date = Date.fromString(scLn.next("Date")+" +0000", "dd MMM yyyy, HH:mm:ss Z")

        val (amount, soldMarket) = split(scLn.next("Sold"))
        val (totalFee, feeMarket) = split(scLn.next("Fee"))

        val token1 = scLn.next("Exchange Rate")
        val (token2, token3) = token1.span(_ != '=')
        val (exchangeRate, _) = split(token3.drop(2))
        val receiverWallet = scLn.next("Receiver Wallet")

        val (amountReceived, receivedMarket) = split(scLn.next("Received"))

        val realFee = amount * exchangeRate - amountReceived
        val feePercent = realFee * 100 / (amount * exchangeRate)

        val desc = "Order: " + receiverWallet

        val exchange =
          Exchange(
            date = date
            , id = receivedMarket
            , fromAmount = amount, fromMarket = Market.normalize(soldMarket)
            , toAmount = amountReceived, toMarket = Market.normalize(receivedMarket)
            , feeAmount = realFee, feeMarket = Market.normalize(feeMarket)
            , exchanger = Changelly
            , description = desc
          )
        return CSVReader.Ok(exchange)
      } else
        return CSVReader.Warning("%s. Read file %s: cannot parse this line: %s.".format(id, Paths.pathFromData(fileName), line))
    }
  }
}



