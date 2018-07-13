package taxes.exchanger

import taxes._
import taxes.date._
import taxes.util.parse.{Scanner, SeparatedScanner, _}


object Poloniex extends Exchanger {
  override val id: String = "Poloniex"

  override val sources = Seq(
      new UserFolderSource[Operation]("poloniex", ".csv") {
        def fileSource(fileName : String) = operationsReader(fileName)
      }
    /* , new UserFolderSource[Operation]("poloniex/borrowing") {
        def fileSource(fileName : String) = borrowingFeesReader(fileName)
      } */
    , new UserFolderSource[Operation]("poloniex/withdrawals", ".csv") {
        def fileSource(fileName : String) = withdrawalsReader(fileName)
      }
    )

  private def operationsReader(fileName : String) = new CSVSortedOperationReader(fileName) {
    override val linesToSkip = 1

    override def lineScanner(line: String) =
      SeparatedScanner(line, "[,%]")

    override def readLine(line: String, scLn: Scanner): CSVReader.Result[Operation] = {
      val date = LocalDateTime.parseAsUTC(scLn.next("Date"), "yyyy-MM-dd HH:mm:ss") // Poloniex csv trade history uses UTC time zone

      val (aux1, aux2) = Parse.split(scLn.next("Pair"), "/")
      val baseMarket = Market.normalize(aux1)
      val quoteMarket = Market.normalize(aux2)

      // BEWARE!!!
      // According to the Wikipedia (https://en.wikipedia.org/wiki/Currency_pair),
      // throughout this program we call quote the market unit used for the price and
      // base the other market in the pair. In other words, price is expressed as a quote per base ratio.
      //
      // So, if price for ETH/BTC pair is 0.07 BTC/ETH, quote is BTC and base is ETH.
      // When you sell a pair, you release coins from the base market and
      // when you buy, you acquire coins from the base market.
      //
      // This seems to be the usual convention but Poloniex uses the opposite one. Nevertheless
      // we still maintain the same terminology used in the rest of this program for Poloniex too.
      // As a consequence:
      // field `Base Total Less Fee` in csv really stands for `Quote Total Less Fee` and
      // field `Quote Total Less Fee` in csv really stands for `Base Total Less Fee`.

      val category = scLn.next("Category")
      val orderType = scLn.next("Order Type")
      val price = scLn.nextDouble("Price")
      val amount = scLn.nextDouble("Amount")
      val total = {
        val t = scLn.nextDouble("Total")
        if(t>0)
          t
        else
          price*amount // if total is less than 0.00000001, it's represented as 0.00000000 in csv file
      }
      val feePercent = scLn.nextDouble("Fee Percent")
      scLn.next("%") // skip %
      val orderNumber = scLn.next("Order Number")
      val baseTotalLessFee = scLn.nextDouble("Base Total Less Fee")
      val quoteTotalLessFee = scLn.nextDouble("Quote Total Less Fee")

      val desc =
        if(orderNumber.nonEmpty)
          "Order: " + orderNumber
        else
          ""

      if(amount==0 && total==0) // Must be a Poloniex error but I got an entry with no amount nor total
        return CSVReader.Warning("%s. Read file %s: Reading this transaction is not currently supported: %s.".format(id, Paths.pathFromData(fileName), line))

      if (category == "Exchange") {
        // Rate is computed as Total / Amount
        // A sell gets you Total minus fee coins. You release Amount coins.
        // A buy gets you Amount minus fee coins. You release Total coins.
        val exchange =
          if (orderType == "Sell") {
            val fee = total * feePercent / 100
            Exchange(
              date = date
              , id = orderNumber
              , fromAmount = amount, fromMarket = baseMarket
              , toAmount = total - fee, toMarket = quoteMarket
              , feeAmount = fee, feeMarket = quoteMarket
              , exchanger = Poloniex
              , description = desc
            )
          } else {
            val fee = amount * feePercent / 100
            Exchange(
              date = date
              , id = orderNumber
              , fromAmount = total, fromMarket = quoteMarket
              , toAmount = amount - fee, toMarket = baseMarket
              , feeAmount = fee, feeMarket = baseMarket
              // Usually, quoteMarket is BTC so we set fee in BTC
              // fee = amount*feePercent/100*price, feeMarket = quoteMarket
              , exchanger = Poloniex
              , description = desc
            )
          }
        return CSVReader.Ok(exchange)
      } else if(category == "Settlement" && orderType == "Buy") {
        // Just like a Exchange buy and then a loss
        val settlement = Exchange(
          date = date
          , id = orderNumber
          , fromAmount = total, fromMarket = quoteMarket
          , toAmount = amount*(100-feePercent)/100, toMarket = baseMarket
          , feeAmount = amount*feePercent/100, feeMarket = baseMarket
          , isSettlement = true
          // Usually, quoteMarket is BTC so we can set fee in BTC
          //, fee = amount*feePercent/100*price, feeMarket = quoteMarket
          , exchanger = Poloniex
          , description = desc + " settlement"
        )
        return CSVReader.Ok(settlement)
      } else if(category == "Margin trade") {
        val margin =
          if (orderType == "Sell") {
            val fee = total * feePercent / 100
            Margin(
              date = date
              , id = orderNumber
              , fromAmount = amount, fromMarket = baseMarket // we short the whole amount but only pay with provided total minus fee
              , toAmount = total - fee /*this total*/ /* baseTotalLessFee */, toMarket = quoteMarket
              , fee = fee, feeMarket = quoteMarket // quoteMarket is usually BTC
              , orderType = Operation.OrderType.Sell
              , pair = (baseMarket, quoteMarket)
              , exchanger = Poloniex
              , description = desc
            )
          } else {
            val fee = amount * feePercent / 100
            Margin(
              date = date
              , id = orderNumber
              , fromAmount = total /*this * (100 - feePercent)/100 */, fromMarket = quoteMarket
              , toAmount = quoteTotalLessFee, toMarket = baseMarket
              , fee = fee, feeMarket = baseMarket
              // Usually, quoteMarket is BTC so we can set fee in BTC
              //, fee = amount*feePercent/100*price, feeMarket = quoteMarket
              , orderType = Operation.OrderType.Buy
              , pair = (baseMarket, quoteMarket)
              , exchanger = Poloniex
              , description = desc
            )
          }
        return CSVReader.Ok(margin)
      } else
        return CSVReader.Warning("%s. Read file %s: Reading this transaction is not currently supported: %s.".format(id, Paths.pathFromData(fileName), line))
    }

    // override def read(): List[Operation] =
    //  group(super.read().sortBy(_.id)).sortBy(_.date)
  }

  def group(operations: List[Operation]) : List[Operation] =
    operations match {
      case (exchange1 : Exchange)::(exchange2 : Exchange)::ops if exchange1.id.nonEmpty && exchange1.id==exchange2.id => {
        val op = exchange1.copy(
            fromAmount = exchange1.fromAmount + exchange2.fromAmount
          , toAmount = exchange1.toAmount + exchange2.toAmount
          , feeAmount = exchange1.feeAmount + exchange2.feeAmount
        )
        group(op :: ops)
      }
      case (margin1 : Margin)::(margin2 : Margin)::ops if margin1.id.nonEmpty && margin1.id==margin2.id => {
        val op = margin1.copy(
            fromAmount = margin1.fromAmount + margin2.fromAmount
          , toAmount = margin1.toAmount + margin2.toAmount
          , fee = margin1.fee + margin2.fee
        )
        group(op :: ops)
      }
      case op1::op2::ops =>
        op1::group(op2::ops)
      case ops =>
        ops
    }

  private def borrowingFeesReader(fileName : String) = new CSVSortedOperationReader(fileName) {
    override val linesToSkip = 1

    override def lineScanner(line: String) =
      SeparatedScanner(line, "[,%]")

    override def readLine(line: String, scLn: Scanner): CSVReader.Result[Operation] = {
      val currency = Market.normalize(scLn.next("Currency"))
      val rate = scLn.nextDouble("Rate")
      val amount = scLn.nextDouble("Amount")
      val duration = scLn.nextDouble("Duration")
      val totalFee = scLn.nextDouble("Total Fee")
      val open = LocalDateTime.parseAsUTC(scLn.next("Open Date"), "yyyy-MM-dd HH:mm:ss") // Poloniex csv borrowing history uses UTC time zone
      val close = LocalDateTime.parseAsUTC(scLn.next("Close Date"), "yyyy-MM-dd HH:mm:ss") // Poloniex csv borrowing history uses UTC time zone

      val desc = "Borrowing fee"

      val fee = Fee(
        date = close
        , id = desc
        , amount = totalFee
        , market = currency
        , exchanger = Poloniex
        , description = desc
      )
      return CSVReader.Ok(fee)
    }
  }

  private def withdrawalsReader(fileName : String) = new CSVSortedOperationReader(fileName) {
    override val linesToSkip = 1

    override def lineScanner(line: String) =
      SeparatedScanner(line, "[,%]")

    override def readLine(line: String, scLn: Scanner): CSVReader.Result[Operation] = {
      val date = LocalDateTime.parseAsUTC(scLn.next("Date"), "yyyy-MM-dd HH:mm:ss") // Poloniex csv withdrawal history uses UTC time zone
      val currency = Market.normalize(scLn.next("Currency"))
      val amount = scLn.nextDouble("Amount")
      val address = scLn.next("Address")
      val status = scLn.next("Status")

      val tk = "COMPLETE: "
      val isFinalized = status.startsWith(tk)

      if(isFinalized) {
        val txid = status.drop(tk.length)

        if(currency == Market.bitcoin) {
          val desc = id + " Withdrawal fee " + currency + " " + txid

          val txInfo = TransactionsCache.lookup(currency, txid, address)

          val totalFee = amount - txInfo.amount

          val fee = Fee(
            date = date
            , id = desc
            , amount = totalFee
            , market = currency
            , exchanger = Poloniex
            , description = desc
          )
          return CSVReader.Ok(fee)
        } else
          CSVReader.Warning("%s. Read withdrawal %s: This withdrawal was ignored: %s.".format(id, Paths.pathFromData(fileName), line))

      } else
        CSVReader.Warning("%s. Read withdrawal %s: This withdrawal was not completed: %s.".format(id, Paths.pathFromData(fileName), line))
    }
  }
}

