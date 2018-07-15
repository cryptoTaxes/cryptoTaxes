package taxes.exchanger

import taxes._
import taxes.date._
import taxes.util.parse._


object Yobit extends Exchanger {
  override val id: String = "Yobit"

  override val sources = Seq(
    new UserFolderSource[Operation]("yobit", ".csv") {
      def fileSource(fileName : String) = operationsReader(fileName)
    }
  )

  private def operationsReader(fileName : String) = new CSVSortedOperationReader(fileName) {
    override val linesToSkip = 1

    override def lineScanner(line: String) =
      SeparatedScanner(line, "[ \t]+")

    override def readLine(line: String, scLn: Scanner): CSVReader.Result[Operation] = {
      val date1 = scLn.next("Date1")
      val date2 = scLn.next("Date2")
      val date3 = scLn.next("Date3")
      val date4 = scLn.next("Date4")

      val date = LocalDateTime.parseAsUTC(date3+" "+date4, "yyyy-MM-dd HH:mm:ss")

      val pair = scLn.next("Pair")
      val orderType = scLn.next("Order Type")
      val price = scLn.nextDouble("Price")
      val amount = scLn.nextDouble("Amount")
      val completed = scLn.nextDouble("Completed")
      val total = scLn.nextDouble("Total")
      scLn.close()

      val desc = ""

      val (m1,m2) = Parse.split(pair, "/")
      val isSell = orderType == "SELL"

      val baseMarket = Market.normalize(m1)
      val quoteMarket = Market.normalize(m2)

      if(completed>0) {
        // quoteMarket is usually BTC
        val exchange =
          if (isSell)
            Exchange(
              date = date
              , id = ""
              , fromAmount = completed, fromMarket = baseMarket
              , toAmount = completed * price, toMarket = quoteMarket
              , feeAmount = 0, feeMarket = quoteMarket
              , exchanger = Yobit
              , description = desc
            )
          else
            Exchange(
              date = date
              , id = ""
              , fromAmount = completed * price, fromMarket = quoteMarket
              , toAmount = completed, toMarket = baseMarket
              , feeAmount = 0, feeMarket = quoteMarket
              , exchanger = Yobit
              , description = desc
            )
        return CSVReader.Ok(exchange)
      } else
        return CSVReader.Ignore
    }
  }
}
