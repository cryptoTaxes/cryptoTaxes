package taxes.Exchanger

import taxes.Util.Parse._
import taxes._

object Poloniex extends Exchanger {
  override val id: String = "Poloniex"

  override val sources = Seq(
      new UserFolderSource[Operation]("poloniex") {
        def fileSource(fileName : String) = operationsReader(fileName)
      }
    , new UserFolderSource[Operation]("poloniex/borrowing") {
        def fileSource(fileName : String) = borrowingFeesReader(fileName)
      }
    , new UserFolderSource[Operation]("poloniex/withdrawals") {
        def fileSource(fileName : String) = withdrawalReader(fileName)
      }
    )

  private def operationsReader(fileName : String) = new CSVSortedOperationReader(fileName) {
    override val hasHeader: Boolean = true

    override def lineScanner(line: String) =
      SeparatedScanner(line, "[,%]")

    override def readLine(line: String, scLn: Scanner): Either[String, Operation] = {
      val date = Date.fromString(scLn.next()+" +0000", "yyyy-MM-dd HH:mm:ss Z") // Poloniex time is 1 hour behind here

      val (market1, aux) = scLn.next().span(_ != '/')
      val market2 = aux.tail

      val category = scLn.next()
      val orderType = scLn.next()
      val price = scLn.nextDouble()
      val amount = scLn.nextDouble()
      val total = scLn.nextDouble()
      val feePercent = scLn.nextDouble()
      scLn.next() // skip %
      val orderNumber = scLn.next()
      val baseTotalLessFee = scLn.nextDouble()
      val quoteTotalLessFee = scLn.nextDouble()
      scLn.close()

      val desc = id + " " + orderNumber

      if (category == "Exchange") {
        val exchange =
          if (orderType == "Sell")
            Exchange(
              date = date
              , id = orderNumber
              , fromAmount = amount, fromMarket = Market.normalize(market1)
              , toAmount = amount*price*(100-feePercent)/100, toMarket = Market.normalize(market2)
              , fee = total*feePercent/100, feeMarket = Market.normalize(market2)
              , exchanger = Poloniex
              , description = desc
            )
          else
            Exchange(
              date = date
              , id = orderNumber
              , fromAmount = total, fromMarket = Market.normalize(market2)
              , toAmount = amount*(100-feePercent)/100, toMarket = Market.normalize(market1)
              // , fee = amount*feePercent/100, feeMarket = CoinConversions.normalize(market1)
              // Usually, market2 is BTC so we set fee in BTC
              , fee = amount*feePercent/100*price, feeMarket = Market.normalize(market2)
              , exchanger = Poloniex
              , description = desc
            )
        return Right(exchange)
      } else if(category == "Settlement" && orderType == "Buy") {
        // Just like a Exchange buy
        val settlement = SettlementBuy(
          date = date
          , id = orderNumber
          , fromAmount = total, fromMarket = Market.normalize(market2)
          , toAmount = amount*(100-feePercent)/100, toMarket = Market.normalize(market1)
          // , fee = amount*feePercent/100, feeMarket = CoinConversions.normalize(market1)
          // Usually, market2 is BTC so we set fee in BTC
          , fee = amount*feePercent/100*price, feeMarket = Market.normalize(market2)
          , exchanger = Poloniex
          , description = desc + " Settlement"
        )
        return Right(settlement)
      } else if(category == "Margin trade") {
        val margin =
          if (orderType == "Sell")
            Margin(
              date = date
              , id = orderNumber
              , fromAmount = amount, fromMarket = Market.normalize(market1)
              , toAmount = amount*price*(100-feePercent)/100, toMarket = Market.normalize(market2)
              , fee = total*feePercent/100, feeMarket = Market.normalize(market2)
              , orderType = Operation.OrderType.Sell
              , pair = (Market.normalize(market1), Market.normalize(market2))
              , exchanger = Poloniex
              , description = desc
            )
          else
            Margin(
              date = date
              , id = orderNumber
              , fromAmount = total, fromMarket = Market.normalize(market2)
              , toAmount = amount*(100-feePercent)/100, toMarket = Market.normalize(market1)
              // , fee = amount*feePercent/100, feeMarket = CoinConversions.normalize(market1)
              // Usually, market2 is BTC so we set fee in BTC
              , fee = amount*feePercent/100*price, feeMarket = Market.normalize(market2)
              , orderType = Operation.OrderType.Buy
              , pair = (Market.normalize(market1), Market.normalize(market2))
              , exchanger = Poloniex
              , description = desc
            )
        return Right(margin)
      } else
        return Left("%s. Read file. Reading this transaction is not currently supported: %s.".format(id, line))
    }

    override def read(): List[Operation] =
      group(super.read().sortBy(_.id)).sortBy(_.date)
  }

  def group(operations: List[Operation]) : List[Operation] =
    operations match {
      case (exchange1 : Exchange)::(exchange2 : Exchange)::ops if exchange1.id.nonEmpty && exchange1.id==exchange2.id => {
        val op = exchange1.copy(
            fromAmount = exchange1.fromAmount + exchange2.fromAmount
          , toAmount = exchange1.toAmount + exchange2.toAmount
          , fee = exchange1.fee + exchange2.fee
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
    override val hasHeader: Boolean = true

    override def lineScanner(line: String) =
      SeparatedScanner(line, "[,%]")

    override def readLine(line: String, scLn: Scanner): Either[String, Operation] = {
      val currency = scLn.next()
      val rate = scLn.nextDouble()
      val amount = scLn.nextDouble()
      val duration = scLn.nextDouble()
      val totalFee = scLn.nextDouble()
      val open = Date.fromString(scLn.next() + " +0000", "yyyy-MM-dd HH:mm:ss Z") // Poloniex time is 1 hour behind here
      val close = Date.fromString(scLn.next() + " +0000", "yyyy-MM-dd HH:mm:ss Z") // Poloniex time is 1 hour behind here

      val desc = id + " Borrowing fees"

      val fee = Fee(
        date = close
        , id = desc
        , amount = totalFee
        , market = Market.normalize(currency)
        , exchanger = Poloniex
        , description = desc
      )
      return Right(fee)
    }
  }

  private def withdrawalReader(fileName : String) = new CSVSortedOperationReader(fileName) {
    override val hasHeader: Boolean = true

    override def lineScanner(line: String) =
      SeparatedScanner(line, "[,%]")

    override def readLine(line: String, scLn: Scanner): Either[String, Operation] = {
      val date = Date.fromString(scLn.next() + " +0000", "yyyy-MM-dd HH:mm:ss Z") // Poloniex time is 1 hour behind here
      val currency = Market.normalize(scLn.next())
      val amount = scLn.nextDouble()
      val address = scLn.next()
      val status = scLn.next()

      val tk = "COMPLETE: "
      val isFinalized = status.startsWith(tk)

      if(isFinalized) {
        val txid = status.drop(tk.length)

        if(currency == Market.bitcoin) {
          val desc = id + " Withdrawal fee " + txid

          val txInfo = TransactionsCache.lookup(currency, txid, address)

          val totalFee = amount - txInfo.amount

          val fee = Fee(
            date = date
            , id = desc
            , amount = totalFee
            , market = Market.normalize(currency)
            , exchanger = Poloniex
            , description = desc
          )
          return Right(fee)
        } else
          Left("%s. Read withdrawal. This withdrawal was ignored: %s.".format(id, line))

      } else
        Left("%s. Read withdrawal. This withdrawal was not completed: %s.".format(id, line))
    }
  }
}

