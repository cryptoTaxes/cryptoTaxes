package taxes.Exchanger

import taxes.Util.Parse._
import taxes._

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
      val date = Date.fromString(scLn.next("Date")+" +0000", "yyyy-MM-dd HH:mm:ss Z") // Poloniex time is 1 hour behind here

      val (aux1, aux2) = scLn.next("Pair").span(_ != '/')
      val market1 = Market.normalize(aux1)
      val market2 = Market.normalize(aux2.tail)

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
              , fromAmount = amount, fromMarket = market1
              , toAmount = total - fee, toMarket = market2
              , feeAmount = fee, feeMarket = market2
              , exchanger = Poloniex
              , description = desc
            )
          } else {
            val fee = amount * feePercent / 100
            Exchange(
              date = date
              , id = orderNumber
              , fromAmount = total, fromMarket = market2
              , toAmount = amount - fee, toMarket = market1
              , feeAmount = fee, feeMarket = market1
              // Usually, market2 is BTC so we set fee in BTC
              // fee = amount*feePercent/100*price, feeMarket = market2
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
          , fromAmount = total, fromMarket = market2
          , toAmount = amount*(100-feePercent)/100, toMarket = market1
          , feeAmount = amount*feePercent/100, feeMarket = market1
          , isSettlement = true
          // Usually, market2 is BTC so we can set fee in BTC
          //, fee = amount*feePercent/100*price, feeMarket = market2
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
              , fromAmount = amount, fromMarket = market1 // we short the whole amount but only pay with provided total minus fee
              , toAmount = total, toMarket = market2
              , fee = fee, feeMarket = market2 // market2 is usually BTC
              , orderType = Operation.OrderType.Sell
              , pair = (market1, market2)
              , exchanger = Poloniex
              , description = desc
            )
          } else {
            val fee = amount * feePercent / 100
            Margin(
              date = date
              , id = orderNumber
              , fromAmount = total * (100 - feePercent)/100, fromMarket = market2
              , toAmount = quoteTotalLessFee, toMarket = market1
              , fee = fee, feeMarket = market1
              // Usually, market2 is BTC so we can set fee in BTC
              //, fee = amount*feePercent/100*price, feeMarket = market2
              , orderType = Operation.OrderType.Buy
              , pair = (market1, market2)
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
      val open = Date.fromString(scLn.next("Open Date") + " +0000", "yyyy-MM-dd HH:mm:ss Z") // Poloniex time is 1 hour behind here
      val close = Date.fromString(scLn.next("Close Date") + " +0000", "yyyy-MM-dd HH:mm:ss Z") // Poloniex time is 1 hour behind here

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
      val date = Date.fromString(scLn.next("Date") + " +0000", "yyyy-MM-dd HH:mm:ss Z") // Poloniex time is 1 hour behind here
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

