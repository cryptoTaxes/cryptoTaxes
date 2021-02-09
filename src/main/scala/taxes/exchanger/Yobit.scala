package taxes.exchanger

import taxes._
import taxes.date._
import taxes.io.FileSystem
import taxes.util.parse._


object Yobit extends Exchanger {
  override val id: String = "Yobit"

  override val sources = Seq(
    new FilteredUserInputFolderSource[Operation]("yobit", ".csv") {
      def fileSource(fileName: String) = operationsReader(fileName)
    },
    new FilteredUserInputFolderSource[Operation]("yobit/deposits", ".csv") {
      def fileSource(fileName: String) = depositsReader(fileName)
    },
    new FilteredUserInputFolderSource[Operation]("yobit/withdrawals", ".csv") {
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

      val desc = RichText("")

      val (currency1, currency2) = Parse.split(pair, "/")
      val isSell = orderType == "SELL"

      val baseCurrency = Currency.normalize(currency1)
      val quoteCurrency = Currency.normalize(currency2)

      if(completed>0) {
        // quoteCurrency is usually BTC
        val exchange =
          if(isSell)
            Exchange(
              date = date
              , id = ""
              , fromAmount = completed, fromCurrency = baseCurrency
              , toAmount = completed * price, toCurrency = quoteCurrency
              , fees = List()
              , exchanger = Yobit
              , description = desc
            )
          else
            Exchange(
              date = date
              , id = ""
              , fromAmount = completed * price, fromCurrency = quoteCurrency
              , toAmount = completed, toCurrency = baseCurrency
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

      val currency = Currency.normalize(scLn.next("Currency"))
      val amount = scLn.nextDouble("Amount")
      val accepted = scLn.next("Status") == "Accepted"
      val txHash = scLn.next("Tx Hash")
      val address = scLn.next("Address")

      val desc =
        if(txHash=="?")
          "Deposit from Yobit"
        else if(address=="?")
          s"Deposit ${RichText.util.transaction(currency, txHash)}"
        else
          s"Deposit ${RichText.util.transaction(currency, txHash, address)}"

      if(accepted) {
        val deposit = Deposit(
          date = date
          , id = ""
          , amount = amount
          , currency = currency
          , exchanger = Yobit
          , address= Some(address)
          , txid = Some(txHash)
          , description = RichText(desc)
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

      val currency = Currency.normalize(scLn.next("Currency"))
      val amount = scLn.nextDouble("Amount")
      val completed = scLn.next("Status") == "Completed"
      val txHash = scLn.next("Tx Hash")
      val address = scLn.next("Address")

      val desc =
        if(txHash=="?")
          "Withdrawal from Yobit"
        else if(address=="?")
          s"Withdrawal ${RichText.util.transaction(currency, txHash)}"
        else
          s"Withdrawal ${RichText.util.transaction(currency, txHash, address)}"

      if(completed) {
        val withdrawal = Withdrawal(
          date = date
          , id = ""
          , amount = amount
          , currency = currency
          , exchanger = Yobit
          , address= Some(address)
          , txid = Some(txHash)
          , description = RichText(desc)
        )
        return CSVReader.Ok(withdrawal)
      } else
        CSVReader.Warning(s"$id. Read withdrawal ${FileSystem.pathFromData(fileName)}: This withdrawal was not completed: $line.")
    }
  }
}
