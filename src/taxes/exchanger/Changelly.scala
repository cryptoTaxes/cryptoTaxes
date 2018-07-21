package taxes.exchanger

import taxes._
import taxes.date._
import taxes.util.Logger
import taxes.util.parse._


object Changelly extends Exchanger {

  override val id: String = "Changelly"

  private def split(str : String) : (Double, Market) = {
    val token = str.filter(_ != ',')
    val sc = SeparatedScanner(token, "[ ]")
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
        val date = LocalDateTime.parseAsUTC(scLn.next("Date"), "dd MMM yyyy, HH:mm:ss")  // Changelly transactions-history.csv file uses UTC time zone
                                                                                         // Note that the detailed transactions shown in the GUI use a different time zone
        val (amount, soldMarket) = split(scLn.next("Sold"))
        val (totalFee, feeMarket) = split(scLn.next("Fee"))

        val (token1, token2) = Parse.split(scLn.next("Exchange Rate"), " = ")
        val (rateSold, soldMarket1) = split(token1)
        val (rateReceived, receivedMarket1) = split(token2)
        val exchangeRate = rateReceived / rateSold

        val receiverWallet = scLn.next("Receiver Wallet")
        val (amountReceived, receivedMarket) = split(scLn.next("Received"))

        if(soldMarket1 != soldMarket || receivedMarket1 != receivedMarket)
          CSVReader.Warning(s"$id. Read file ${FileSystem.pathFromData(fileName)}: cannot parse this line: $line as rate is not expressed as receivedMarket/soldMarket.")

        val realFee = amount * exchangeRate - amountReceived
        val feePercent = realFee * 100 / (amount * exchangeRate)

        val desc = "Order: " + receiverWallet

        val exchange =
          Exchange(
            date = date
            , id = receivedMarket
            , fromAmount = amount, fromMarket = Market.normalize(soldMarket)
            , toAmount = amountReceived, toMarket = Market.normalize(receivedMarket)
            , fees = List(FeePair(realFee, Market.normalize(feeMarket)))
            , exchanger = Changelly
            , description = desc
          )
        return CSVReader.Ok(exchange)
      } else
        return CSVReader.Warning(s"$id. Read file ${FileSystem.pathFromData(fileName)}: cannot parse this line: $line.")
    }
  }
}



