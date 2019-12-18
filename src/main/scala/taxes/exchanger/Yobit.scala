package taxes.exchanger

import taxes._
import taxes.date._
import taxes.io.FileSystem
import taxes.util.parse._


object Yobit extends Exchanger {
  override val id: String = "Yobit"

  override val sources = Seq(
    new UserInputFolderSource[Operation]("yobit", ".csv") {
      def fileSource(fileName: String) = operationsReader(fileName)
    },
    new UserInputFolderSource[Operation]("yobit/deposits", ".csv") {
      def fileSource(fileName: String) = depositsReader(fileName)
    },
    new UserInputFolderSource[Operation]("yobit/withdrawals", ".csv") {
      def fileSource(fileName: String) = withdrawalsReader(fileName)
    }
  )

  private def operationsReader(fileName: String) = new CSVSortedOperationReader(fileName) {
    override val linesToSkip = 1

    override def lineScanner(line: String) =
      SeparatedScanner(line, "[ \t]+")

    override def readLine(line: String, scLn: Scanner): CSVReader.Result[Operation] = {
      val date1 = scLn.next("Date1")
      val date2 = scLn.next("Date2")
      val date3 = scLn.next("Date3")
      val date4 = scLn.next("Date4")

      val date = LocalDateTime.parseAsUTC(s"$date3 $date4", "yyyy-MM-dd HH:mm:ss")

      val pair = scLn.next("Pair")
      val orderType = scLn.next("Order Type")
      val price = scLn.nextDouble("Price")
      val amount = scLn.nextDouble("Amount")
      val completed = scLn.nextDouble("Completed")
      val total = scLn.nextDouble("Total")

      val desc = ""

      val (m1,m2) = Parse.split(pair, "/")
      val isShort = orderType == "SELL"

      val baseMarket = Market.normalize(m1)
      val quoteMarket = Market.normalize(m2)

      if(completed>0) {
        // quoteMarket is usually BTC
        val exchange =
          if(isShort)
            Exchange(
              date = date
              , id = ""
              , fromAmount = completed, fromMarket = baseMarket
              , toAmount = completed * price, toMarket = quoteMarket
              , fees = List()
              , exchanger = Yobit
              , description = desc
            )
          else
            Exchange(
              date = date
              , id = ""
              , fromAmount = completed * price, fromMarket = quoteMarket
              , toAmount = completed, toMarket = baseMarket
              , fees = List()
              , exchanger = Yobit
              , description = desc
            )
        return CSVReader.Ok(exchange)
      } else
        return CSVReader.Ignore
    }
  }

  private def depositsReader(fileName: String) = new CSVSortedOperationReader(fileName) {
    override val linesToSkip = 1

    override def lineScanner(line: String) =
      SeparatedScanner(line, "[ \t]+")

    override def readLine(line: String, scLn: Scanner): CSVReader.Result[Operation] = {
      val date1 = scLn.next("Date1")
      val date2 = scLn.next("Date2")

      val date = LocalDateTime.parseAsUTC(s"$date1 $date2", "yyyy-MM-dd HH:mm:ss")

      val currency = Market.normalize(scLn.next("Currency"))
      val amount = scLn.nextDouble("Amount")
      val accepted = scLn.next("Status") == "Accepted"

      if(accepted) {
        val deposit = Deposit(
          date = date
          , id = ""
          , amount = amount
          , market = currency
          , exchanger = Yobit
          , description = "Deposit into Yobit"
        )
        return CSVReader.Ok(deposit)
      } else
        CSVReader.Warning(s"$id. Read deposit ${FileSystem.pathFromData(fileName)}: This deposit was not completed: $line.")
    }
  }

  private def withdrawalsReader(fileName: String) = new CSVSortedOperationReader(fileName) {
    override val linesToSkip = 1

    override def lineScanner(line: String) =
      SeparatedScanner(line, "[ \t]+")

    override def readLine(line: String, scLn: Scanner): CSVReader.Result[Operation] = {
      val date1 = scLn.next("Date1")
      val date2 = scLn.next("Date2")

      val date = LocalDateTime.parseAsUTC(s"$date1 $date2", "yyyy-MM-dd HH:mm:ss")

      val currency = Market.normalize(scLn.next("Currency"))
      val amount = scLn.nextDouble("Amount")
      val completed = scLn.next("Status") == "Completed"

      if(completed) {
        val withdrawal = Withdrawal(
          date = date
          , id = ""
          , amount = amount
          , market = currency
          , exchanger = Yobit
          , description = "Withdrawal from Yobit"
        )
        return CSVReader.Ok(withdrawal)
      } else
        CSVReader.Warning(s"$id. Read withdrawal ${FileSystem.pathFromData(fileName)}: This withdrawal was not completed: $line.")
    }
  }
}
