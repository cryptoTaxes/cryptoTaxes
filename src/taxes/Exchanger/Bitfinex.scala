package taxes.Exchanger

import taxes._
import taxes.Util.Parse.{CSVReader, CSVSortedOperationReader, Scanner, SeparatedScanner}

object Bitfinex extends Exchanger {
  override val id: String = "Bitfinex"

  override val sources = Seq(
    new UserFolderSource[Operation]("bitfinex", ".csv") {
      def fileSource(fileName: String) = reportReader(fileName)
    },
    new UserFolderSource[Operation]("bitfinex/ledger", ".csv") {
      def fileSource(fileName: String) = ledgerReader(fileName)
    }
  )

  private def reportReader(fileName: String) = new CSVSortedOperationReader(fileName) {
    override val hasHeader: Boolean = true

    override def lineScanner(line: String) =
      SeparatedScanner(line, "[,]")

    override def readLine(line: String, scLn: Scanner): CSVReader.Result[Operation] = {
      val reference = scLn.next("Reference")

      val (aux1, aux2) = scLn.next("Pair").splitAt(3)
      val market1 = Market.normalize(aux1)
      val market2 = Market.normalize(aux2)

      val orderType = scLn.next("Order Type")
      val hidden = scLn.next("Hidden")

      val amount = scLn.nextDouble("Amount")
      val amountExecuted = scLn.nextDouble("Amount Executed")

      val price = scLn.next("Price")
      val averageExecutionPrice = scLn.nextDouble("Average Execution Price")

      val created = Date.fromString(scLn.next("Date Created"), "yyyy-MM-dd HH:mm:ss.S")
      val updated = Date.fromString(scLn.next("Date Updated"), "yyyy-MM-dd HH:mm:ss.S")

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
                , fee = 0, feeMarket = Market.normalize(market1)
                , exchanger = Bitfinex
                , description = desc
              )
            else
              Exchange( // we are buying BTC with $
                date = updated
                , id = reference
                , fromAmount = averageExecutionPrice * amountExecuted.abs, fromMarket = Market.normalize(market2)
                , toAmount = amountExecuted.abs, toMarket = Market.normalize(market1)
                , fee = 0, feeMarket = Market.normalize(market1)
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
                  , fee = 0.abs, feeMarket = Market.normalize(market1)
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
                  , fee = 0.abs, feeMarket = Market.normalize(market1)
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
    override val hasHeader: Boolean = true

    override def lineScanner(line: String) =
      SeparatedScanner(line, "[,]")

    override def readLine(line: String, scLn: Scanner): CSVReader.Result[Operation] = {
      val currency = Market.normalize(scLn.next("Currency"))
      val description = scLn.next("Description")
      val amount = scLn.nextDouble("Amount")
      val balance = scLn.next("Balance")
      val date = Date.fromString(scLn.next("Date"), "yyyy-MM-dd HH:mm:ss")

      val desc = description

      if (description.startsWith("Settlement") && currency == Market.usd) {
        // USD settlements are really paid as BTC settlements
        return CSVReader.Ignore
      } else if (description.startsWith("Settlement") && currency == Market.bitcoin) {
        val token1 = description.dropWhile(_ != '@').tail.tail
        val token2 = token1.takeWhile(_ != ' ')
        val sc = SeparatedScanner(token2, "[ ]")
        val price = sc.nextDouble()
        sc.close()

        val settlement =
          SettlementBuy(
            date = date
            , id = description
            , fromAmount = amount.abs, fromMarket = currency
            , toAmount = amount.abs * price, toMarket = Market.usd
            , fee = 0, feeMarket = currency
            , exchanger = Bitfinex
            , description = desc
          )
        return CSVReader.Ok(settlement)
      } else if(description.startsWith("Trading fee") ||
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
      } else
        return CSVReader.Warning("%s. Read file %s: Reading this transaction is not currently supported: %s.".format(id, Paths.pathFromData(fileName), line))
    }
  }
}
