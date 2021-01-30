package taxes.exchanger

import taxes._
import taxes.date._
import taxes.io.FileSystem
import taxes.util.parse._


object Bitfinex extends Exchanger {
  override val id: String = "Bitfinex"

  override val sources = Seq(
    new FilteredUserInputFolderSource[Operation]("bitfinex/ledger", ".csv") {
      def fileSource(fileName: String) = ledgerReader(fileName)
    },
    new FilteredUserInputFolderSource[Operation]("bitfinex/trades", ".csv") {
      def fileSource(fileName: String) = tradesReader(fileName)
    },
    new FilteredUserInputFolderSource[Operation]("bitfinex/deposits", ".csv") {
      def fileSource(fileName: String) = depositsReader(fileName)
    },
    new FilteredUserInputFolderSource[Operation]("bitfinex/withdrawals", ".csv") {
      def fileSource(fileName: String) = withdrawalsReader(fileName)
    }
  )

  // note that Bitfinex processes margin in a LIFO way
  override val marginLongs = StackStockPool()
  override val marginShorts = StackStockPool()

  private def tradesReader(fileName: String) = new CSVSortedOperationReader(fileName) {
    override val linesToSkip = 1

    override def lineScanner(line: String) =
      SeparatedScanner(line, "[,]")

    override def readLine(line: String, scLn: Scanner): CSVReader.Result[Operation] = {
      val reference = scLn.next("Reference")

      val (aux1, aux2) = Parse.split(scLn.next("Pair"), "/")
      val baseCurrency = Currency.normalize(aux1)
      val quoteCurrency = Currency.normalize(aux2)

      val amount = scLn.nextDouble("Amount")

      val price = scLn.nextDouble("Price")

      val fee = scLn.nextDouble("Fee")

      val feeCurrency = Currency.normalize(scLn.next("FeeCurrency"))

      val date =
        if(Config.config.deprecatedUp2017Version)
          LocalDateTime.parseAsMyZoneId(scLn.next("Date Created"), "yyyy-MM-dd HH:mm:ss")
        else
          LocalDateTime.parseAsUTC(scLn.next("Date Created"), "yyyy-MM-dd HH:mm:ss")

      val margin = scLn.next("Margin")

      val desc = s"Order: $reference"

      if(margin.toLowerCase == "false") { // exchange order
        val exchange =
          if(amount<0)
            Exchange( // we are selling BTC for $. Fee can be in BTC or in $
              date = date
              , id = reference
              , fromAmount = amount.abs - (if(feeCurrency==baseCurrency) fee.abs else 0)
              , fromCurrency = baseCurrency
              , toAmount = price * amount.abs - (if(feeCurrency==quoteCurrency) fee.abs else 0)
              , toCurrency = quoteCurrency
              , fees = List(FeePair(fee.abs, feeCurrency))
              , exchanger = Bitfinex
              , description = RichText(desc)
            )
          else
            Exchange( // we are buying BTC with $. Fee can be in BTC or in $
              date = date
              , id = reference
              , fromAmount = price * amount.abs - (if(feeCurrency==quoteCurrency) fee.abs else 0)
              , fromCurrency = quoteCurrency
              , toAmount = amount.abs - (if(feeCurrency==baseCurrency) fee.abs else 0)
              , toCurrency = baseCurrency
              , fees = List(FeePair(fee.abs, feeCurrency))
              , exchanger = Bitfinex
              , description = RichText(desc)
            )
        return CSVReader.Ok(exchange)
      } else if(margin.toLowerCase == "true") { // margin order
        val margin =
          if(amount<0)
            Margin(
              date = date
              , id = reference
              , fromAmount = amount.abs
              , fromCurrency = baseCurrency
              , toAmount = price * amount.abs /* + (if(feeCurrency==quoteCurrency) fee.abs else if(feeCurrency==baseCurrency) -fee.abs * price else 0) */
              , toCurrency = quoteCurrency
              , fees = List(FeePair(fee.abs, feeCurrency))
              , orderType = Operation.OrderType.Sell
              , pair = (baseCurrency, quoteCurrency)
              , exchanger = Bitfinex
              , description = RichText(s"$desc @ $price")
            )
          else
            Margin(
              date = date
              , id = reference
              , fromAmount = amount.abs * price /* + (if(feeCurrency==quoteCurrency) fee.abs else if(feeCurrency==baseCurrency) -fee.abs * price else 0) */
              , fromCurrency = quoteCurrency
              , toAmount = amount.abs
              , toCurrency = baseCurrency
              , fees = List(FeePair(fee.abs, feeCurrency))
              , orderType = Operation.OrderType.Buy
              , pair = (baseCurrency, quoteCurrency)
              , exchanger = Bitfinex
              , description = RichText(s"$desc @ $price")
            )

          return CSVReader.Ok(margin)
      }
      else
        return CSVReader.Warning(s"$id. Read file ${FileSystem.pathFromData(fileName)}: Reading this transaction is not currently supported: $line. Margin should be true or false but it is $margin")
    }
  }

  private def reportReader(fileName: String) = new CSVSortedOperationReader(fileName) {
    override val linesToSkip = 1

    override def lineScanner(line: String) =
      SeparatedScanner(line, "[,]")

    override def readLine(line: String, scLn: Scanner): CSVReader.Result[Operation] = {
      val reference = scLn.next("Reference")

      val (m1, m2) = scLn.next("Pair").splitAt(3)
      val currency1 = Currency.normalize(m1)
      val currency2 = Currency.normalize(m2)

      val orderType = scLn.next("Order Type")
      val hidden = scLn.next("Hidden")

      val amount = scLn.nextDouble("Amount")
      val amountExecuted = scLn.nextDouble("Amount Executed")

      val price = scLn.next("Price")
      val averageExecutionPrice = scLn.nextDouble("Average Execution Price")

      val created = LocalDateTime.parse(scLn.next("Date Created"), "yyyy-MM-dd HH:mm:ss.S")
      val updated = LocalDateTime.parse(scLn.next("Date Updated"), "yyyy-MM-dd HH:mm:ss.S")

      val status = scLn.next("Status")

      val desc = RichText(s"Order: $reference")

      if(amountExecuted != 0) { //status.startsWith("EXECUTED") || status.contains("PARTIALLY FILLED")) {
        if(orderType.contains("EXCHANGE")) {
          val exchange =
            if(amountExecuted>0)
              Exchange( // we are selling BTC for $
                date = updated
                , id = reference
                , fromAmount = amountExecuted.abs
                , fromCurrency = Currency.normalize(currency1)
                , toAmount = averageExecutionPrice * amountExecuted.abs
                , toCurrency = Currency.normalize(currency2)
                , fees = List()
                , exchanger = Bitfinex
                , description = desc
              )
            else
              Exchange( // we are buying BTC with $
                date = updated
                , id = reference
                , fromAmount = averageExecutionPrice * amountExecuted.abs
                , fromCurrency = Currency.normalize(currency2)
                , toAmount = amountExecuted.abs
                , toCurrency = Currency.normalize(currency1)
                , fees = List()
                , exchanger = Bitfinex
                , description = desc
              )
            return CSVReader.Ok(exchange)
        } else { // margin order
            val margin =
              if(amount<0) // Beware: cannot use amountExecuted
                Margin(
                  date = updated
                  , id = reference
                  , fromAmount = amountExecuted.abs
                  , fromCurrency = Currency.normalize(currency1)
                  , toAmount = amountExecuted.abs*averageExecutionPrice - 0.abs
                  , toCurrency = Currency.normalize(currency2) // Assumes feeCurrency == currency2
                  , fees = List()
                  , orderType = Operation.OrderType.Sell
                  , pair = (currency1, currency2)
                  , exchanger = Bitfinex
                  , description = desc
                )
              else
                Margin(
                  date = updated
                  , id = reference
                  , fromAmount = amountExecuted.abs * averageExecutionPrice
                  , fromCurrency = Currency.normalize(currency2)
                  , toAmount = amountExecuted.abs
                  , toCurrency = Currency.normalize(currency1)
                  , fees = List()
                  , orderType = Operation.OrderType.Buy
                  , pair = (currency1, currency2)
                  , exchanger = Bitfinex
                  , description = desc
                )
          return CSVReader.Ok(margin)
        }
      }
      return CSVReader.Ignore
    }
  }

  private def ledgerReader(fileName: String) = new CSVSortedOperationReader(fileName) {
    override val linesToSkip = 1

    override def lineScanner(line: String) =
      SeparatedScanner(line, "[,]")

    override def readLine(line: String, scLn: Scanner): CSVReader.Result[Operation] = {
      val currency = Currency.normalize(scLn.next("Currency"))
      val description = scLn.next("Description")
      val amount = scLn.nextDouble("Amount")
      val balance = scLn.next("Balance")

      val date =
        if(Config.config.deprecatedUp2017Version)
          LocalDateTime.parseAsMyZoneId(scLn.next("Date Created"), "yyyy-MM-dd HH:mm:ss")
        else
          LocalDateTime.parseAsUTC(scLn.next("Date Created"), "yyyy-MM-dd HH:mm:ss")

      val desc = RichText(description)

      if(description.startsWith("Settlement") && currency == Currency.usd) {
        // USD settlements are really paid as BTC settlements
        return CSVReader.Ignore
      } else if(description.startsWith("Settlement") && currency == Currency.bitcoin) {
        val token1 = description.dropWhile(_ != '@').tail.tail
        val token2 = token1.takeWhile(_ != ' ')
        val sc = SeparatedScanner(token2, "[ ]")
        val price = sc.nextDouble()
        sc.close()

        val settlement = {
          val total = amount.abs * price
          val fee = 0.2 * total / 100
          Exchange(
            date = date.plusSeconds(-240) // This is a bit ad-hoc but goal is to place settlement before it's used for paying a fee/loss
            , id = description
            , fromAmount = amount.abs
            , fromCurrency = currency
            , toAmount = total - fee
            , toCurrency = Currency.usd
            , fees = List(FeePair(fee, Currency.usd))
            , isSettlement = true
            , exchanger = Bitfinex
            , description = desc
          )
        }
        return CSVReader.Ok(settlement)
      } /* else if(description.startsWith("Trading fee") ||
                (description.contains("Position") &&
                  (description.contains("funding") || description.contains("swap")))) {
        val fee =
          Fee(
            date
            , description
            , amount.abs
            , currency
            , Bitfinex
            , desc
          )
        return CSVReader.Ok(fee)
      } */ else
        return CSVReader.Warning(s"$id. Read file ${FileSystem.pathFromData(fileName)}: Reading this transaction is not currently supported: $line.")
    }
  }

  private def depositsReader(fileName: String) = new CSVSortedOperationReader(fileName) {
    override val linesToSkip = 1

    override def lineScanner(line: String) =
      SeparatedScanner(line, "[,]")

    override def readLine(line: String, scLn: Scanner): CSVReader.Result[Operation] = {
      val reference = scLn.next("Reference")
      val currency = Currency.normalize(scLn.next("Currency"))
      val method = scLn.next("Method")
      val amount = scLn.nextDouble("Amount")
      val completed = scLn.next("Status") == "COMPLETED"
      val address = scLn.next("Address")
      val txid = scLn.next("TXID")
      val created = LocalDateTime.parseAsUTC(scLn.next("Created"), "yyyy-MM-dd HH:mm:ss")
      val updated = LocalDateTime.parseAsUTC(scLn.next("Updated"), "yyyy-MM-dd HH:mm:ss")

      if(completed) {
        val desc = RichText(s"Deposit ${RichText.util.transaction(currency, txid, address)}")
        val deposit = Deposit(
          date = created
          , id = txid
          , amount = amount
          , currency = currency
          , exchanger = Bitfinex
          , description = desc
        )
        return CSVReader.Ok(deposit)
      } else
        CSVReader.Warning(s"$id. Read deposit ${FileSystem.pathFromData(fileName)}: This deposit was not completed: $line.")
    }
  }

  private def withdrawalsReader(fileName: String) = new CSVSortedOperationReader(fileName) {
    override val linesToSkip = 1

    override def lineScanner(line: String) =
      SeparatedScanner(line, "[,]")

    override def readLine(line: String, scLn: Scanner): CSVReader.Result[Operation] = {
      val reference = scLn.next("Reference")
      val currency = Currency.normalize(scLn.next("Currency"))
      val method = scLn.next("Method")
      val amount = scLn.nextDouble("Amount")
      val completed = scLn.next("Status") == "COMPLETED"
      val address = scLn.next("Address")
      val txid = scLn.next("TXID")
      val created = LocalDateTime.parseAsUTC(scLn.next("Created"), "yyyy-MM-dd HH:mm:ss")
      val updated = LocalDateTime.parseAsUTC(scLn.next("Updated"), "yyyy-MM-dd HH:mm:ss")

      if(completed) {
        val desc = RichText(s"Withdrawal ${RichText.util.transaction(currency, txid, address)}")
        val withdrawal = Withdrawal(
          date = created
          , id = txid
          , amount = amount.abs
          , currency = currency
          , exchanger = Bitfinex
          , description = desc
        )
        return CSVReader.Ok(withdrawal)
      } else
        CSVReader.Warning(s"$id. Read withdrawal ${FileSystem.pathFromData(fileName)}: This withdrawal was not completed: $line.")
    }
  }
}