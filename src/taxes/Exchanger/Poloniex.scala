package taxes.Exchanger

import taxes._

object Poloniex extends Exchanger {
  override val id: String = "Poloniex"

  override val folder: String = "poloniex"

  def readFile(fileName : String) : List[Operation] = {
    val f = new java.io.File(fileName)
    val sc = new java.util.Scanner(f)
    var operations = List[Operation]()
    val header = sc.nextLine()
    while(sc.hasNextLine) {
      val ln = ParseUtils.trimSpaces(sc.nextLine())
      if(ln.nonEmpty) {
        val scLn = new java.util.Scanner(ln).useDelimiter("[,%]")
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
          operations ::= exchange
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
          operations ::= settlement
        } else if(category == "Margin trade") {
          val exchange =
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
            operations ::= exchange
        } else
          Logger.warning("%s.readExchanges. Reading this transaction is not currently supported: %s.".format(id,ln))
      }
    }
    sc.close()
    return group(operations.sortBy(_.id)).sortBy(_.date)
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
}

