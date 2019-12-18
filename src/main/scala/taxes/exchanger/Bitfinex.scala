package taxes.exchanger

import taxes._
import taxes.date._
import taxes.io.FileSystem
import taxes.util.parse._


object Bitfinex extends Exchanger {
  override val id: String = "Bitfinex"

  override val sources = Seq(
    new UserInputFolderSource[Operation]("bitfinex/ledger", ".csv") {
      def fileSource(fileName: String) = ledgerReader(fileName)
    },
    new UserInputFolderSource[Operation]("bitfinex/trades", ".csv") {
      def fileSource(fileName: String) = tradesReader(fileName)
    },
    new UserInputFolderSource[Operation]("bitfinex/deposits", ".csv") {
      def fileSource(fileName: String) = depositsReader(fileName)
    },
    new UserInputFolderSource[Operation]("bitfinex/withdrawals", ".csv") {
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
      val baseMarket = Market.normalize(aux1)
      val quoteMarket = Market.normalize(aux2)

      val amount = scLn.nextDouble("Amount")

      val price = scLn.nextDouble("Price")

      val fee = scLn.nextDouble("Fee")

      val feeCurrency = scLn.next("feeCurrency")
      val feeMarket = Market.normalize(feeCurrency)

      val date =
        if(Config.config.deprecatedUp2017Version)
          LocalDateTime.parseAsMyZoneId(scLn.next("Date Created"), "yyyy-MM-dd HH:mm:ss")
        else
          LocalDateTime.parseAsUTC(scLn.next("Date Created"), "yyyy-MM-dd HH:mm:ss")

      val margin = scLn.next("Margin")

      val desc = "Order: " + reference

      if(margin.toLowerCase == "false") { // exchange order
        val exchange =
          if(amount<0)
            Exchange( // we are selling BTC for $. Fee can be in BTC or in $
              date = date
              , id = reference
              , fromAmount = amount.abs - (if(feeMarket==baseMarket) fee.abs else 0), fromMarket = baseMarket
              , toAmount = price * amount.abs - (if(feeMarket==quoteMarket) fee.abs else 0), toMarket = quoteMarket
              , fees = List(FeePair(fee.abs, feeMarket))
              , exchanger = Bitfinex
              , description = desc
            )
          else
            Exchange( // we are buying BTC with $. Fee can be in BTC or in $
              date = date
              , id = reference
              , fromAmount = price * amount.abs - (if(feeMarket==quoteMarket) fee.abs else 0), fromMarket = quoteMarket
              , toAmount = amount.abs - (if(feeMarket==baseMarket) fee.abs else 0), toMarket = baseMarket
              , fees = List(FeePair(fee.abs, feeMarket))
              , exchanger = Bitfinex
              , description = desc
            )
        return CSVReader.Ok(exchange)
      } else if(margin.toLowerCase == "true") { // margin order
        val margin =
          if(amount<0)
            Margin(
              date = date
              , id = reference
              , fromAmount = amount.abs, fromMarket = baseMarket
              , toAmount = price * amount.abs /* + (if(feeMarket==quoteMarket) fee.abs else if(feeMarket==baseMarket) -fee.abs * price else 0) */, toMarket = quoteMarket
              , fees = List(FeePair(fee.abs, feeMarket))
              , orderType = Operation.OrderType.Sell
              , pair = (baseMarket, quoteMarket)
              , exchanger = Bitfinex
              , description = desc + " @ " + price
            )
          else
            Margin(
              date = date
              , id = reference
              , fromAmount = amount.abs * price /* + (if(feeMarket==quoteMarket) fee.abs else if(feeMarket==baseMarket) -fee.abs * price else 0) */, fromMarket = quoteMarket
              , toAmount = amount.abs, toMarket = baseMarket
              , fees = List(FeePair(fee.abs, feeMarket))
              , orderType = Operation.OrderType.Buy
              , pair = (baseMarket, quoteMarket)
              , exchanger = Bitfinex
              , description = desc + " @ " + price
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
      val market1 = Market.normalize(m1)
      val market2 = Market.normalize(m2)

      val orderType = scLn.next("Order Type")
      val hidden = scLn.next("Hidden")

      val amount = scLn.nextDouble("Amount")
      val amountExecuted = scLn.nextDouble("Amount Executed")

      val price = scLn.next("Price")
      val averageExecutionPrice = scLn.nextDouble("Average Execution Price")

      val created = LocalDateTime.parse(scLn.next("Date Created"), "yyyy-MM-dd HH:mm:ss.S")
      val updated = LocalDateTime.parse(scLn.next("Date Updated"), "yyyy-MM-dd HH:mm:ss.S")

      val status = scLn.next("Status")

      val desc = "Order: " + reference

      if(amountExecuted != 0) { //status.startsWith("EXECUTED") || status.contains("PARTIALLY FILLED")) {
        if(orderType.contains("EXCHANGE")) {
          val exchange =
            if(amountExecuted>0)
              Exchange( // we are selling BTC for $
                date = updated
                , id = reference
                , fromAmount = amountExecuted.abs, fromMarket = Market.normalize(market1)
                , toAmount = averageExecutionPrice * amountExecuted.abs, toMarket = Market.normalize(market2)
                , fees = List()
                , exchanger = Bitfinex
                , description = desc
              )
            else
              Exchange( // we are buying BTC with $
                date = updated
                , id = reference
                , fromAmount = averageExecutionPrice * amountExecuted.abs, fromMarket = Market.normalize(market2)
                , toAmount = amountExecuted.abs, toMarket = Market.normalize(market1)
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
                  , fromAmount = amountExecuted.abs, fromMarket = Market.normalize(market1)
                  , toAmount = amountExecuted.abs*averageExecutionPrice - 0.abs , toMarket = Market.normalize(market2) // Assumes feeCurrency == market2
                  , fees = List()
                  , orderType = Operation.OrderType.Sell
                  , pair = (market1, market2)
                  , exchanger = Bitfinex
                  , description = desc
                )
              else
                Margin(
                  date = updated
                  , id = reference
                  , fromAmount = amountExecuted.abs * averageExecutionPrice, fromMarket = Market.normalize(market2)
                  , toAmount = amountExecuted.abs, toMarket = Market.normalize(market1)
                  , fees = List()
                  , orderType = Operation.OrderType.Buy
                  , pair = (market1, market2)
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
      val currency = Market.normalize(scLn.next("Currency"))
      val description = scLn.next("Description")
      val amount = scLn.nextDouble("Amount")
      val balance = scLn.next("Balance")

      val date =
        if(Config.config.deprecatedUp2017Version)
          LocalDateTime.parseAsMyZoneId(scLn.next("Date Created"), "yyyy-MM-dd HH:mm:ss")
        else
          LocalDateTime.parseAsUTC(scLn.next("Date Created"), "yyyy-MM-dd HH:mm:ss")

      val desc = description

      if(description.startsWith("Settlement") && currency == Market.usd) {
        // USD settlements are really paid as BTC settlements
        return CSVReader.Ignore
      } else if(description.startsWith("Settlement") && currency == Market.bitcoin) {
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
            , fromAmount = amount.abs, fromMarket = currency
            , toAmount = total - fee, toMarket = Market.usd
            , fees = List(FeePair(fee, Market.usd))
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
      val currency = Market.normalize(scLn.next("Currency"))
      val method = scLn.next("Method")
      val amount = scLn.nextDouble("Amount")
      val completed = scLn.next("Status") == "COMPLETED"
      val address = scLn.next("Address")
      val txid = scLn.next("TXID")
      val created = LocalDateTime.parseAsUTC(scLn.next("Created"), "yyyy-MM-dd HH:mm:ss")
      val updated = LocalDateTime.parseAsUTC(scLn.next("Updated"), "yyyy-MM-dd HH:mm:ss")

      if(completed) {
        val desc = "Deposit " + address + "\n" + txid
        val deposit = Deposit(
          date = created
          , id = txid
          , amount = amount
          , market = currency
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
      val currency = Market.normalize(scLn.next("Currency"))
      val method = scLn.next("Method")
      val amount = scLn.nextDouble("Amount")
      val completed = scLn.next("Status") == "COMPLETED"
      val address = scLn.next("Address")
      val txid = scLn.next("TXID")
      val created = LocalDateTime.parseAsUTC(scLn.next("Created"), "yyyy-MM-dd HH:mm:ss")
      val updated = LocalDateTime.parseAsUTC(scLn.next("Updated"), "yyyy-MM-dd HH:mm:ss")

      if(completed) {
        val desc = "Withdrawal " + AddressBook.format(address) + "\n" + txid
        val withdrawal = Withdrawal(
          date = created
          , id = txid
          , amount = amount.abs
          , market = currency
          , exchanger = Bitfinex
          , description = desc
        )
        return CSVReader.Ok(withdrawal)
      } else
        CSVReader.Warning(s"$id. Read withdrawal ${FileSystem.pathFromData(fileName)}: This withdrawal was not completed: $line.")
    }
  }
}