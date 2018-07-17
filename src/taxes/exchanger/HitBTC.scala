package taxes.exchanger

import taxes._
import taxes.date._
import taxes.util.parse._


object HitBTC extends Exchanger {
  override val id: String = "HitBTC"

  override val sources = Seq(
    new UserFolderSource[Operation]("hitbtc", ".csv") {
      def fileSource(fileName : String) = operationsReader(fileName)
    }
  )

  private def operationsReader(fileName : String) = new CSVSortedOperationReader(fileName) {
    override val linesToSkip = 1

    override def lineScanner(line: String): Scanner =
      QuotedScanner(line, '\"', ',')

    // HitBTC shows time offset used for dates in csv header line. We take it from there
    private lazy val offset = skippedLines(0).dropWhile(_ != '(').tail.takeWhile(_ != ')')

    override def readLine(line: String, scLn: Scanner): CSVReader.Result[Operation] = {
      val date = LocalDateTime.parse(scLn.next("Date")+offset, "yyyy-MM-dd HH:mm:ssX")
      val instrument = scLn.next("Instrument")
      val tradeID = scLn.next("Trade ID")
      val orderID = scLn.next("Order ID")
      val side = scLn.next("Side")
      val quantity = scLn.nextDouble("Quantity")
      val price = scLn.nextDouble("Price")
      val volume = scLn.nextDouble("Volume")
      val fee = scLn.nextDouble("Fee")
      val rebate = scLn.nextDouble("Rebate")
      val total = scLn.nextDouble("Total")

      val desc = "Order: " + tradeID + "/" + orderID

      val (m1,m2) = Parse.split(instrument,"/")
      val baseMarket = Market.normalize(m1)
      val quoteMarket = Market.normalize(m2)
      val isSell = side == "sell"

      // quoteMarket is usually BTC
      if(isSell) {
        val exchange = Exchange(
          date = date
          , id = tradeID + "/" + orderID
          , fromAmount = quantity, fromMarket = baseMarket
          , toAmount = volume - fee + rebate, toMarket = quoteMarket
          , fees = List(FeePair(fee, quoteMarket))
          , exchanger = HitBTC
          , description = desc
        )

        return CSVReader.Ok(exchange)
      } else //toDo add support for buy orders
        return CSVReader.Warning("%s. Read file %s: Reading this transaction is not currently supported: %s.".format(id, Paths.pathFromData(fileName), line))
    }
  }
}
