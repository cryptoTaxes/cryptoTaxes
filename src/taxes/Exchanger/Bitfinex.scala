package taxes.Exchanger

import taxes._
import taxes.Util.Parse.{CSVReader, CSVSortedOperationReader, Scanner, SeparatedScanner}

object Bitfinex extends Exchanger {
  override val id: String = "Bitfinex"

  override val sources = Seq(
    new UserFolderSource[Operation]("bitfinex") {
      def fileSource(fileName: String) = reportReader(fileName)
    },
    new UserFolderSource[Operation]("bitfinex/ledger") {
      def fileSource(fileName: String) = ledgerReader(fileName)
    }
  )

  private def operationsReader(fileName: String) = new CSVSortedOperationReader(fileName) {
    override val hasHeader: Boolean = true

    override def lineScanner(line: String) =
      SeparatedScanner(line, "[,]")

    override def readLine(line: String, scLn: Scanner): CSVReader.Result[Operation] = {
      val reference = scLn.next()

      val (aux1, aux2) = scLn.next().span(_ != '/')
      val market1 = Market.normalize(aux1)
      val market2 = Market.normalize(aux2.tail)

      val amount = scLn.nextDouble()
      val price = scLn.nextDouble()
      val feeAmount = scLn.nextDouble()
      val feeCurrency = Market.normalize(scLn.next())
      val date = Date.fromString(scLn.next(), "yyyy-MM-dd HH:mm:ss")

      val desc = Bitfinex + " " + reference

      if(feeCurrency == market1) {
        if(amount>0) {
          // we are buying $ with BTC
          val exchange =
            Exchange( // we are selling BTC for $
              date = date
              , id = reference
              , fromAmount = amount.abs, fromMarket = Market.normalize(market1)
              , toAmount = amount.abs*price, toMarket = Market.normalize(market2)
              , fee = 0, feeMarket = Market.normalize(feeCurrency)
              , exchanger = Bitfinex
              , description = desc
            )
          return CSVReader.Ok(exchange)
        } else {
          val exchange =
            Exchange( // we are buying BTC with $
              date = date
              , id = reference
              , fromAmount = amount.abs*price, fromMarket = Market.normalize(market2)
              , toAmount = amount.abs, toMarket = Market.normalize(market1)
              , fee = 0, feeMarket = Market.normalize(feeCurrency)
              , exchanger = Bitfinex
              , description = desc
            )
          return CSVReader.Ok(exchange)
        }
      }
      val margin =
        if(amount<0)
          Margin(
            date = date
            , id = reference
            , fromAmount = amount.abs, fromMarket = Market.normalize(market1)
            , toAmount = amount.abs*price - feeAmount.abs , toMarket = Market.normalize(market2) // Assumes feeCurrency == market2
            , fee = feeAmount.abs, feeMarket = Market.normalize(feeCurrency)
            , orderType = Operation.OrderType.Sell
            , pair = (market1, market2)
            , exchanger = Bitfinex
            , description = desc
          )
        else
          Margin(
            date = date
            , id = reference
            , fromAmount = amount.abs * price, fromMarket = Market.normalize(market2)
            , toAmount = amount.abs, toMarket = Market.normalize(market1)
            , fee = feeAmount.abs, feeMarket = Market.normalize(feeCurrency)
            , orderType = Operation.OrderType.Buy
            , pair = (market1, market2)
            , exchanger = Bitfinex
            , description = desc
          )
      return CSVReader.Ok(margin)
    }
  }

  private def reportReader(fileName: String) = new CSVSortedOperationReader(fileName) {
    override val hasHeader: Boolean = true

    override def lineScanner(line: String) =
      SeparatedScanner(line, "[,]")

    override def readLine(line: String, scLn: Scanner): CSVReader.Result[Operation] = {
      val reference = scLn.next()

      val (aux1, aux2) = scLn.next().splitAt(3)
      val market1 = Market.normalize(aux1)
      val market2 = Market.normalize(aux2)

      val orderType = scLn.next()
      val hidden = scLn.next()

      val amount = scLn.nextDouble()
      val amountExecuted = scLn.nextDouble()

      val price = scLn.next()
      val averageExecutionPrice = scLn.nextDouble()

      val created = Date.fromString(scLn.next(), "yyyy-MM-dd HH:mm:ss.S")
      val updated = Date.fromString(scLn.next(), "yyyy-MM-dd HH:mm:ss.S")

      val status = scLn.next()

      val desc = Bitfinex + " " + reference


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
        } else {
            // margin
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
      val currency = Market.normalize(scLn.next())
      val description = scLn.next()
      val amount = scLn.nextDouble()
      val balance = scLn.next()
      val date = Date.fromString(scLn.next(), "yyyy-MM-dd HH:mm:ss")

      val desc = Bitfinex + " " + description

      if (description.startsWith("Settlement") && currency == Market.usd) {
        // USD settlements are really paid as BTC settlements
        return CSVReader.Ignore

      } else if (description.startsWith("Settlement") && currency == Market.bitcoin) {
        val token1 = description.dropWhile(_ != '@').tail.tail
        val token2 = token1.takeWhile(_ != ' ')
        println(token1, token2)
        val sc = SeparatedScanner(token2, "[ ]")
        val price = sc.nextDouble()
        sc.close()

        val settlement = SettlementBuy(
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
                (description.contains("Position") && (description.contains("funding") || description.contains("swap")))) {
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
        return CSVReader.Warning("%s. Read file. Reading this transaction is not currently supported: %s.".format(id, line))
    }
  }
}
