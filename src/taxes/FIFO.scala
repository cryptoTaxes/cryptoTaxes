package taxes

import java.io.PrintStream
import java.text.SimpleDateFormat
import taxes.Exchanger._
import taxes.Market.Market
import taxes.Util.Logger


object Format {
  val header : String = "".padTo(80,'=')
  
  val df = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")

  def asMarket(number : Double, marketUnit : Market) : String = {
    val fmt =
      if(marketUnit == Market.euro)
        EuroBaseCoin.format
      else if(marketUnit == Market.usd)
        USDBaseCoin.format
      else
        BTCBaseCoin.format
    fmt.format(number)+" %s".format(marketUnit)
  }

  def leftPad(str : String, len : Int, c : Char) : String =
    "".padTo(len-str.length, c)+str

  private val tabs = Array(17,75)

  class ExtendedString(value: String) {
    def tab(n:Int) = value.padTo(tabs(n), ' ')
  }

  implicit def extendString(str: String) = new ExtendedString(str)
}


case class ValueTracker(baseMarket : Market)  {
  private val map = scala.collection.mutable.Map[Market,Double]()

  def clear(): Unit = {
    map.clear()
  }

  def record(market : Market, amount : Double): Unit = {
    map += (market -> (amount + map.getOrElse(market, 0.0)))
  }

  def sum : Double = map.values.sum

  def keys : Iterable[Market] =
    map.keys

  def iterator : Iterator[(Market, Double)] =
    map.iterator

  def apply(market : Market) : Double =
    map.getOrElse(market, 0.0)

  def printOn(ps : PrintStream): Unit = {
    var sum = 0.0
    for((label,total) <- map.toList.sortBy(_._2)) {
      sum += total
      ps.println(
        "%-6s ".format(label+":")+" "+Format.leftPad(Format.asMarket(total, baseMarket), 20, ' ')+
        " " + Format.leftPad(Format.asMarket(sum, baseMarket), 20 , ' ')
      )
    }
    ps.println("Total: "+Format.asMarket(map.values.sum, baseMarket))
  }
}


object FIFO {
  import Format.extendString

  def process(): Unit = {
    val config = Config.config

    val operations = Exchangers.readAllOperations().sortBy(_.date)

    if(operations.isEmpty)
      Logger.fatal("No operation was found in any exchange for user: "+config.user+".")

    val outFile = Paths.userOutputFolder+"/output.txt"
    val out = new java.io.PrintStream(outFile)

    val csv = CSV()

    // what market is our basis
    val baseCoin = config.baseCoin
    val baseMarket = baseCoin.market

    // current stock of assets with their cost bases (expressed in base coin)
    val stocks = QueueStockPool(baseMarket)

    // current in longs/shorts assets for margin trading operations
    val marginBuysMap = scala.collection.immutable.Map[Exchanger, StockPool](
        Poloniex -> QueueStockPool(Market.bitcoin)
      , Bitfinex -> StackStockPool(Market.usd)
      )
    val marginSellsMap = scala.collection.immutable.Map[Exchanger, StockPool](
        Poloniex -> QueueStockPool(Market.bitcoin)
      , Bitfinex -> StackStockPool(Market.usd)
      )

    var frees = List[String]()
    var price0 = List[Exchange]()


    // All quantities expressed in base coin
    object Realized {
      // record realized gains/losses/paid fees per market
      val perMarketGains = ValueTracker(baseMarket)
      val perMarketLooses = ValueTracker(baseMarket)
      val perMarketPaidFees = ValueTracker(baseMarket) // fees are already deducted from gains and added to looses

      // Difference between these is realized gains
      val costBases = ValueTracker(baseMarket)  // cost bases of sold coins
      val sellValues = ValueTracker(baseMarket) // value obtained for sold coins
    }


    // order of current operation within current year
    var operationNumber = 0


    def initializeYear(year : Int): Int = {
      Realized.perMarketGains.clear()
      Realized.perMarketLooses.clear()
      Realized.perMarketPaidFees.clear()

      Realized.costBases.clear()
      Realized.sellValues.clear()

      frees = List[String]()
      price0 = List[Exchange]()

      operationNumber = 0

      out.println(Format.header)
      out.println(year)
      out.println(Format.header)
      out.println()

      val csvFile = Paths.userOutputFolder+"/output%d.csv".format(year)
      csv.setOutputTo(csvFile)

      for(i <- 1 to 1)
        csv.println()
      csv.println(year)
      csv.printlnHeader

      return year
    }


    def reportPortfolio(): Unit = {
      out.println("End of year portfolio")
      out.println(Format.header)
      out.println("Market        Amount           Total Cost")
      out.println(Format.header)
      var totalCost = 0.0
      for(queue <- stocks.iterator.toList.sortBy(_.market)) {
        var amount = 0.0
        var cost = 0.0
        for(stock <- queue) {
          amount += stock.amount
          cost += stock.amount * stock.costBasis
        }
        totalCost += cost
        if(cost > 0.001) {
          out.print(
            "%-6s %15.5f  ".format(queue.market+":", amount)+
            Format.leftPad(Format.asMarket(cost, baseMarket), 20, ' ')+
            " at "+Format.leftPad(Format.asMarket(cost/amount,baseMarket)+"/%s  ".format(queue.market), 24, ' ')+
            "    "
          )
          out.println(queue)
        }
      }
      out.println(Format.header)
      out.println("Portfolio total cost: "+Format.asMarket(totalCost, baseMarket))
    }


    def reportYear(year : Int): Unit = {
      out.println(Format.header)
      out.println(year)
      out.println(Format.header)
      out.println()

      reportPortfolio()

      if(Config.verbosity(Verbosity.showMoreDetails)) {
        out.println()
        out.println()
        out.println("FREES")
        for (f <- frees.reverse)
          out.println(f)

        out.println()
        out.println()
        out.println("PRICE0")
        for (op <- price0.reverse)
          out.println(op)
      }

      out.println()
      out.println(Format.header)
      out.println("Gains per market")
      out.println(Format.header)
      Realized.perMarketGains.printOn(out)

      out.println()
      out.println(Format.header)
      out.println("Looses per market")
      out.println(Format.header)
      Realized.perMarketLooses.printOn(out)

      out.println()
      out.println(Format.header)
      out.println("Paid fees per market")
      out.println(Format.header)
      Realized.perMarketPaidFees.printOn(out)
      out.println()

      out.print("Net result:      ")
      out.println(Format.asMarket(Realized.perMarketGains.sum - Realized.perMarketLooses.sum, baseMarket))
      out.println()

      out.println(Format.header)
      out.println("Realized gains/looses")
      out.println(Format.header)
      out.println()
      out.println(Format.header)
      out.println("Cost bases per market")
      out.println(Format.header)
      Realized.costBases.printOn(out)
      out.println()
      out.println(Format.header)
      out.println("Sell values per market")
      out.println(Format.header)
      Realized.sellValues.printOn(out)
      out.println()

      out.println(Format.header)
      out.println("Realized Gains per market")
      out.println(Format.header)
      val realizedGains = {
        val keys = Realized.costBases.keys.toSet union Realized.sellValues.keys.toSet
        val list = keys.map(k => (k, Realized.sellValues(k) - Realized.costBases(k)))
        val valueTracker = ValueTracker(baseMarket)
        for((k,v) <- list)
          valueTracker.record(k,v)
        valueTracker
      }
      realizedGains.printOn(out)

      out.println()

      out.println()
      out.print("Realized total cost basis:  ")
      out.println(Format.leftPad(Format.asMarket(Realized.costBases.sum, baseMarket), 20, ' '))
      out.print("Realized total sell value:  ")
      out.println(Format.leftPad(Format.asMarket(Realized.sellValues.sum, baseMarket), 20, ' '))
      out.print("Net result:                 ")
      out.println(Format.leftPad(Format.asMarket(Realized.sellValues.sum - Realized.costBases.sum, baseMarket), 20, ' '))
      out.println()

      if(Config.verbosity(Verbosity.showMoreDetails)) {
        out.println("Opened margin longs:")
        for(marginBuy <- marginBuysMap.values)
          for (cont <- marginBuy)
            if(cont.totalAmount>0)
              out.println(cont)

        out.println("Opened margin shorts:")
        for(marginSell <- marginSellsMap.values)
          for (cont <- marginSell)
            if(cont.totalAmount>0)
              out.println(cont)
        out.println()
      }
    }


    def processExchange(exchange: Exchange) {
      val soldMarket = exchange.fromMarket
      val soldAmount = exchange.fromAmount

      val boughtMarket = exchange.toMarket
      val boughtAmount = exchange.toAmount

      // Compute exchange rates
      val (boughtSoldExchangeRate, soldBoughtExchangeRate) =
        if (exchange.fee == 0)
          (boughtAmount / soldAmount, soldAmount / boughtAmount)
        else if (exchange.feeMarket == boughtMarket)
          ((boughtAmount + exchange.fee) / soldAmount, soldAmount / (boughtAmount + exchange.fee))
        else if (exchange.feeMarket == soldMarket)
          (boughtAmount / (soldAmount - exchange.fee), (soldAmount - exchange.fee) / boughtAmount)
        else
          Logger.fatal("Cannot compute exchange rates as fee is not expressed in same unit as from or to units " + exchange.toString)

      // the fee for this operation, expressed in base coin
      val feeInBaseCoin =
        if (exchange.fee == 0)
          0
        else if (exchange.feeMarket == baseMarket)
          exchange.fee
        else if (exchange.feeMarket == boughtMarket && soldMarket == baseMarket)
          soldBoughtExchangeRate * exchange.fee
        else if (exchange.feeMarket == soldMarket && boughtMarket == baseMarket)
          boughtSoldExchangeRate * exchange.fee
        else if (exchange.feeMarket == boughtMarket) {
          if (Market.priority(soldMarket) > Market.priority(boughtMarket)) {
            val total = soldAmount * baseCoin.priceInBaseCoin(soldMarket, exchange.date)
            exchange.fee * total / (boughtAmount+exchange.fee)
          } else {
            val total = (boughtAmount+exchange.fee) * baseCoin.priceInBaseCoin(boughtMarket, exchange.date)
            exchange.fee * total / (boughtAmount+exchange.fee)
          }
        } else if (exchange.feeMarket == boughtMarket) {
          if (Market.priority(soldMarket) > Market.priority(boughtMarket)) {
            val total = soldAmount * baseCoin.priceInBaseCoin(soldMarket, exchange.date)
            exchange.fee * total / (boughtAmount+exchange.fee)
          } else {
            exchange.fee * baseCoin.priceInBaseCoin(boughtMarket, exchange.date)
          }
        } else if (exchange.feeMarket == soldMarket) {
          if (Market.priority(soldMarket) > Market.priority(boughtMarket)) {
            exchange.fee * baseCoin.priceInBaseCoin(soldMarket, exchange.date)
          } else {
            val boughtInBaseCoin = boughtAmount * baseCoin.priceInBaseCoin(boughtMarket, exchange.date)
            boughtInBaseCoin / (soldAmount / exchange.fee - 1)
          }
        } else
          exchange.fee * baseCoin.priceInBaseCoin(exchange.feeMarket, exchange.date)

      Realized.perMarketPaidFees.record(exchange.feeMarket, feeInBaseCoin)

      // Total value involved in this operation, expressed in base coin
      val (totalInBaseCoin, baseCoinProxy) =
        if (soldMarket == baseMarket)
          (soldAmount, baseMarket)
        else if (boughtMarket == baseMarket)
          (boughtAmount + feeInBaseCoin, baseMarket)
        else if (Market.priority(soldMarket) > Market.priority(boughtMarket))
          (soldAmount * baseCoin.priceInBaseCoin(soldMarket, exchange.date), soldMarket)
        else
          ( boughtAmount * baseCoin.priceInBaseCoin(boughtMarket, exchange.date) + feeInBaseCoin // bought amount does not include fee
          , boughtMarket
          )

      // Price we sold released coins at, expressed in base coin
      val soldPriceInBaseCoin = totalInBaseCoin / soldAmount

      // Price we bought acquired coins at, expressed in base coin
      val boughtBasisPriceInBaseCoin = (totalInBaseCoin - feeInBaseCoin) / boughtAmount

      // Add bought coins with their cost basis to our stock of coins
      if (!boughtBasisPriceInBaseCoin.isNaN() && boughtAmount > 0) {
        stocks.add(boughtMarket, boughtAmount, boughtBasisPriceInBaseCoin, exchange.exchanger, exchange.date)
      }

      // Get cost basis for sold coins from current stocks
      val (soldBasisInBaseCoin, noBasis) =
        if (soldMarket != baseMarket)
          stocks.remove(soldMarket, soldAmount)
        else {
          stocks.remove(soldMarket, soldAmount)
          (soldAmount, 0.0)
        }

      // Total value of sold coins, expressed in base coin
      val sellValueInBaseCoin = totalInBaseCoin

      // Gain or fee in this exchange, expressed in base coin
      val gainInBaseCoin =
        if (soldMarket == baseMarket) // If base coin is Euros, buying with euros makes no profit
          if (feeInBaseCoin == 0)
            0
          else
            -feeInBaseCoin
        else
          sellValueInBaseCoin - soldBasisInBaseCoin - feeInBaseCoin

      // Update total gains for soldMarket
      if (gainInBaseCoin > 0) {
        Realized.perMarketGains.record(soldMarket, gainInBaseCoin)
      } else if (gainInBaseCoin < 0) {
        Realized.perMarketLooses.record(soldMarket, gainInBaseCoin.abs)
      }

      // Update realized cost bases and sell values
      if(soldMarket == baseMarket) {
        // toDo this is the case for Euro -> BTC if your base is Euro.
        // toDO Should the fee be charged to Euro instead?
        Realized.costBases.record(boughtMarket, feeInBaseCoin)

      } else {
        Realized.costBases.record(soldMarket, soldBasisInBaseCoin)
        Realized.sellValues.record(soldMarket, sellValueInBaseCoin - feeInBaseCoin)
      }

      if (soldBasisInBaseCoin == 0 && (soldMarket != baseMarket))
        price0 ::= exchange

      if (noBasis.abs > 0.01 && (soldMarket != baseMarket))
      //toDO fix me
        frees ::= "SOLD %.8f %.6f %s = %.8f %s  ".format(noBasis, soldAmount, soldMarket, noBasis * sellValueInBaseCoin / soldAmount, baseMarket)


      if (true || (exchange.fromMarket=="XLM" || exchange.toMarket=="XLM")) {
        val qSold = stocks(soldMarket)
        val qBought = stocks(boughtMarket)

        out.println(
          "%d. ".format(operationNumber)+
            Format.df.format(exchange.date)+
            ". Exchange "+
            Format.asMarket(soldAmount, soldMarket)+" -> "+
            Format.asMarket(boughtAmount, boughtMarket)+
            ". Fee = "+Format.asMarket(exchange.fee, exchange.feeMarket)+
            ". "+exchange.description
        )
        if(Config.verbosity(Verbosity.showRates)) {
          out.println(
            "EXCHANGE RATES:".tab(0) + Format.asMarket(boughtSoldExchangeRate, boughtMarket) +
            "/%s   ".format(soldMarket) +
            Format.asMarket(soldBoughtExchangeRate, soldMarket) + "/%s".format(boughtMarket)
          )
          out.println(
            "%s RATE:".format(baseMarket).tab(0) +
            Format.asMarket(baseCoin.priceInBaseCoin(baseCoinProxy, exchange.date), baseMarket) +
            "/" + baseCoinProxy
          )
        }
        if(Config.verbosity(Verbosity.showDetails))
          out.print((
            "SOLD:".tab(0)+Format.asMarket(soldAmount, soldMarket)+
            " at "+Format.asMarket(soldPriceInBaseCoin, baseMarket)+
            "/%s".format(soldMarket)+
            " = "+Format.asMarket(sellValueInBaseCoin, baseMarket)).tab(1)
          )
        if(Config.verbosity(Verbosity.showStocks))
          out.println(qSold)
        else if(Config.verbosity(Verbosity.showDetails))
          out.println()
        if(Config.verbosity(Verbosity.showMoreDetails))
          out.println(
            "COST BASIS:".tab(0)+Format.asMarket(soldBasisInBaseCoin,baseMarket)
          )
        if(Config.verbosity(Verbosity.showDetails))
          if(!boughtBasisPriceInBaseCoin.isNaN()) {
            out.print((
              "BOUGHT:".tab(0) + Format.asMarket(boughtAmount, boughtMarket)+
              " at " + Format.asMarket(boughtBasisPriceInBaseCoin, baseMarket)+
              "/%s".format(boughtMarket)).tab(1)
            )
            if(Config.verbosity(Verbosity.showStocks))
              out.println(qBought)
            else
              out.println()
          }
        if(Config.verbosity(Verbosity.showMoreDetails) && exchange.fee > 0)
          out.println(
            "FEE:".tab(0)+Format.asMarket(exchange.fee, exchange.feeMarket)+
            " = "+Format.asMarket(feeInBaseCoin,baseMarket)
          )
        out.print((if(gainInBaseCoin>0) "GAIN:" else "LOSS:").tab(0))
        if(Config.verbosity(Verbosity.showDetails))
          if (soldMarket != baseMarket) // If base coin is Euros, buying with euros makes no profit
            out.print(
              Format.asMarket(sellValueInBaseCoin,baseMarket)+
                " - " + Format.asMarket(soldBasisInBaseCoin,baseMarket)+
                (if(exchange.fee >0)
                  " - " + Format.asMarket(feeInBaseCoin,baseMarket)
                 else
                  ""
                )+" = "
            )
        out.println(Format.asMarket(gainInBaseCoin,baseMarket))
        out.println()

        val csvEntry = csv.Entry(
            date = Some(exchange.date)
          , sold = Some(soldMarket)
          , soldAmount = Some(soldAmount)
          , bought = Some(boughtMarket)
          , boughtAmount = Some(boughtAmount)
          , costBasis = Some(soldBasisInBaseCoin)
          , sellValue = Some(sellValueInBaseCoin)
          , fee = Some(feeInBaseCoin)
        )

        csv.println(csvEntry)
      }
    }


    def processFee(fee : Fee) {
      val (feeCostInBaseCoin,_) = stocks.remove(fee.market, fee.amount)

      Realized.perMarketPaidFees.record(fee.market, feeCostInBaseCoin)

      // Paying a fee implies loosing paid amount.
      // Update total looses for soldMarket
      Realized.perMarketLooses.record(fee.market, feeCostInBaseCoin)

      // Record cost basis of paid coins
      Realized.costBases.record(fee.market, feeCostInBaseCoin)

      out.println(
        "%d. ".format(operationNumber)+
        Format.df.format(fee.date)+
        ". Fee "+
        Format.asMarket(fee.amount, fee.market)+
        ". "+fee.description
      )
      if(Config.verbosity(Verbosity.showMoreDetails)) {
        out.print(
          ("PAID FEE:".tab(0) + Format.asMarket(fee.amount, fee.market)).tab(1)
        )
        if(Config.verbosity(Verbosity.showStocks))
          out.println(stocks(fee.market))
        else
          out.println()
        out.println(
          "FEE COST BASIS:".tab(0) + Format.asMarket(feeCostInBaseCoin, baseMarket)
        )
      }
      out.print(
        ("FEE:".tab(0)+Format.asMarket(-feeCostInBaseCoin, baseMarket)).tab(1)
      )
      if(Config.verbosity(Verbosity.showStocks))
        out.println(stocks(fee.market))
      else
        out.println()
      out.println()

      if(feeCostInBaseCoin>0) {
        val csvEntry = csv.Entry(
            date = Some(fee.date)
          , sold = Some("Fee")
          , fee = Some(feeCostInBaseCoin)
        )

        csv.println(csvEntry)
      }
    }


    def processLoss(loss : Loss) {
      val (feeBasisInBaseCoin,_) = stocks.remove(loss.feeMarket, loss.fee)
      Realized.perMarketPaidFees.record(loss.feeMarket, feeBasisInBaseCoin)

      val (basisInBaseCoin,_) = stocks.remove(loss.market, loss.amount)

      // take fee into account
      val totalLossInBaseCoin = basisInBaseCoin + feeBasisInBaseCoin

      // Update total looses for loss market
      Realized.perMarketLooses.record(loss.market, totalLossInBaseCoin)

      // Record cost bases of lost coins and paid fee
      Realized.costBases.record(loss.market, basisInBaseCoin)
      Realized.costBases.record(loss.feeMarket, feeBasisInBaseCoin)

      out.println(
        "%d. ".format(operationNumber)+
          Format.df.format(loss.date)+
          ". Loss "+Format.asMarket(loss.amount, loss.market)+
          " . Fee = "+Format.asMarket(loss.fee, loss.feeMarket)+
          ". "+loss.description
      )
      if(Config.verbosity(Verbosity.showMoreDetails)) {
        out.print((
          "PAID:".tab(0) + Format.asMarket(loss.amount, loss.market)).tab(1)
        )
        if(Config.verbosity(Verbosity.showStocks))
          out.println(stocks(loss.market))
        else
          out.println()
        out.println(
          "COST BASIS:".tab(0) + Format.asMarket(basisInBaseCoin, baseMarket)
        )
      }
      if(Config.verbosity(Verbosity.showMoreDetails)) {
        out.print((
          "PAID FEE:".tab(0) + Format.asMarket(loss.fee, loss.feeMarket)).tab(1)
        )
        if(Config.verbosity(Verbosity.showStocks))
          out.println(stocks(loss.feeMarket))
        else if(Config.verbosity(Verbosity.showMoreDetails))
          out.println()
        out.println(
          "FEE COST BASIS:".tab(0) + Format.asMarket(feeBasisInBaseCoin, baseMarket)
        )
      }

      if(feeBasisInBaseCoin>0)
        out.println(
          "FEE:".tab(0) + Format.asMarket(-feeBasisInBaseCoin, baseMarket)
        )
      out.print((
        "LOSS:".tab(0) + Format.asMarket(-basisInBaseCoin, baseMarket)).tab(1)
      )
      if(Config.verbosity(Verbosity.showStocks))
        out.println(stocks(loss.market))
      else
        out.println()
      out.println()

      val csvEntry = csv.Entry(
          date = Some(loss.date)
        , sold = Some(loss.market)
        , soldAmount = Some(loss.amount)
        , bought = Some("Loss")
        , costBasis = Some(basisInBaseCoin)
        , fee = Some(feeBasisInBaseCoin)
      )

      csv.println(csvEntry)
    }


    def processGain(gain : Gain) {
      val (feeBasisInBaseCoin,_) = stocks.remove(gain.feeMarket, gain.fee)
      Realized.perMarketPaidFees.record(gain.feeMarket, feeBasisInBaseCoin)

      // Paying a fee implies loosing paid amount
      Realized.perMarketLooses.record(gain.feeMarket, feeBasisInBaseCoin)

      // Record cost basis of gained coins
      val basePrice = baseCoin.priceInBaseCoin(gain.market, gain.date)
      stocks.add(gain.market, gain.amount, basePrice, gain.exchanger, gain.date)

      val gainInBaseCoin = gain.amount*basePrice

      // Update total gains for corresponding market
      Realized.perMarketGains.record(gain.market, gainInBaseCoin)

      // Record cost basis of paid fee and make the gain accountable now (by considering it as a sell)
      Realized.costBases.record(gain.feeMarket, feeBasisInBaseCoin)
      Realized.sellValues.record(gain.market, gainInBaseCoin)

      out.println(
        "%d. ".format(operationNumber)+
        Format.df.format(gain.date)+
        ". Gain "+Format.asMarket(gain.amount, gain.market)+" "+
        ". Fee = "+Format.asMarket(gain.fee, gain.feeMarket)+
        ". "+gain.description
      )

      if(Config.verbosity(Verbosity.showMoreDetails)) {
        out.print((
          "RECEIVED:".tab(0) + Format.asMarket(gain.amount, gain.market) +
          " at " + Format.asMarket(basePrice, baseMarket) +
          "/%s".format(gain.market) +
          " = " + Format.asMarket(gainInBaseCoin, baseMarket)).tab(1)
        )
        if(Config.verbosity(Verbosity.showStocks))
          out.println(stocks(gain.market))
        else
          out.println()
        out.print((
          "PAID FEE:".tab(0) + Format.asMarket(gain.fee, gain.feeMarket)).tab(1)
        )
        if(Config.verbosity(Verbosity.showStocks))
          out.println(stocks(gain.feeMarket))
        else
          out.println()
        out.println(
          "FEE COST BASIS:".tab(0) + Format.asMarket(feeBasisInBaseCoin, baseMarket)
        )
      }
      if(feeBasisInBaseCoin>0)
        out.println(
          "FEE:".tab(0) + Format.asMarket(-feeBasisInBaseCoin, baseMarket)
        )
      out.print(
        "GAIN:".tab(0)
      )
      out.println(
        Format.asMarket(gainInBaseCoin, baseMarket)
      )
      out.println()

      val csvEntry = csv.Entry(
          date = Some(gain.date)
        , sold = Some("Gain")
        , bought = Some(gain.market)
        , boughtAmount = Some(gain.amount)
        , sellValue = Some(gainInBaseCoin)
        , fee = Some(feeBasisInBaseCoin)
      )

      csv.println(csvEntry)
    }


    def marginPairKey(market1 : Market, market2: Market) : String
      = market1 ++ "/" ++ market2

    def processSettlement(settlement: SettlementBuy) {
      val soldMarket = settlement.fromMarket
      val soldAmount = settlement.fromAmount

      val boughtMarket = settlement.toMarket
      val boughtAmount = settlement.toAmount

      val tradingBaseMarket = settlement.fromMarket

      // Compute exchange rates
      val (boughtSoldExchangeRate, soldBoughtExchangeRate) =
        if (settlement.feeMarket == boughtMarket)
          ((boughtAmount + settlement.fee) / soldAmount, soldAmount / (boughtAmount + settlement.fee))
        else if (settlement.feeMarket == soldMarket)
          (boughtAmount / (soldAmount - settlement.fee), (soldAmount - settlement.fee) / boughtAmount)
        else
          Logger.fatal("processSettlement. Cannot compute exchange rates as fee is not expressed in same unit as from or to units " + settlement.toString)

      // the fee for this operation, expressed in base coin
      val feeInTradingBaseCoin =
        if (settlement.fee == 0)
          0
        else if (settlement.feeMarket == tradingBaseMarket)
          settlement.fee
        else if (settlement.feeMarket == settlement.toMarket && settlement.fromMarket == tradingBaseMarket)
          soldBoughtExchangeRate * settlement.fee
        else
          settlement.fee * baseCoin.priceInBaseCoin(settlement.feeMarket, settlement.date)

      // Total value involved in this operation, expressed in base coin
      val totalInTradingBaseCoin =
        if (soldMarket == tradingBaseMarket)
          soldAmount
        else if (boughtMarket == tradingBaseMarket)
          boughtAmount + feeInTradingBaseCoin
        else if (Market.priority(soldMarket) > Market.priority(boughtMarket))
          soldAmount * baseCoin.priceInBaseCoin(soldMarket, settlement.date)
        else
          boughtAmount * baseCoin.priceInBaseCoin(boughtMarket, settlement.date) + feeInTradingBaseCoin // bought amount does not include fee

      val marketKey = marginPairKey(settlement.toMarket, settlement.fromMarket)

      out.println(
        "%d. ".format(operationNumber)+
        Format.df.format(settlement.date)+
        ". Settlement "+
        Format.asMarket(soldAmount, soldMarket)+" -> "+
        Format.asMarket(boughtAmount, boughtMarket)+
        ". Fee = "+Format.asMarket(settlement.fee, settlement.feeMarket)+
        ". "+settlement.description
      )
      if(Config.verbosity(Verbosity.showRates))
        out.println((
          "EXCHANGE RATES: "+Format.asMarket(boughtSoldExchangeRate,boughtMarket)+
          "/%s   ".format (soldMarket)+
          Format.asMarket(soldBoughtExchangeRate,soldMarket)+"/%s".format(boughtMarket)).tab(1)
        )

      // This is like a loss but we have to remove bought coins from stock of opened shorts
      // (we bought them to pay for a short that went against us)
      if(settlement.exchanger == Poloniex) {
        val stockContainer = marginSellsMap(Poloniex)(marketKey)
        if (stockContainer.nonEmpty) {
          if (Config.verbosity(Verbosity.showStocks)) {
            out.print("".tab(1))
            out.println(stockContainer)
          }

          stockContainer.removeAndGetBasis(boughtAmount)

          if (Config.verbosity(Verbosity.showStocks)) {
            out.print("".tab(1))
            out.println(stockContainer)
          }
        }
      }
      out.println()

      processLoss(Loss(settlement.date, settlement.id, totalInTradingBaseCoin, tradingBaseMarket, 0/*feeInTradingBaseCoin*/, tradingBaseMarket, settlement.exchanger, settlement.description))
    }


    def processMargin(margin: Margin) {
      val soldMarket = margin.fromMarket
      val soldAmount = margin.fromAmount

      val boughtMarket = margin.toMarket
      val boughtAmount = margin.toAmount

      val (tradedMarket, tradingBaseMarket) = margin.pair

      val marketKey = marginPairKey(tradedMarket, tradingBaseMarket)

      val marginBuys = marginBuysMap(margin.exchanger)
      val marginSells = marginSellsMap(margin.exchanger)

      // Check that markets involved in margin operation are those in traded pair
      if(margin.orderType == Operation.OrderType.Sell) {
        if(soldMarket != tradedMarket || boughtMarket != tradingBaseMarket)
          Logger.fatal("Wrong markets in margin sell: "+margin)
      } else if(margin.orderType == Operation.OrderType.Buy) {
        if(soldMarket != tradingBaseMarket || boughtMarket != tradedMarket)
          Logger.fatal("Wrong markets in margin buy: "+margin)
      }

      // Compute exchange rates
      val (boughtSoldExchangeRate, soldBoughtExchangeRate) =
        if (margin.feeMarket == boughtMarket)
          ((boughtAmount + margin.fee) / soldAmount, soldAmount / (boughtAmount + margin.fee))
        else if (margin.feeMarket == soldMarket)
          (boughtAmount / (soldAmount - margin.fee), (soldAmount - margin.fee) / boughtAmount)
        else
          Logger.fatal("Cannot compute exchange rates as fee is not expressed in same unit as from or to units " + margin.toString)

      // The fee for this operation, expressed in trading base coin
      val feeInTradingBaseCoin =
        if (margin.fee == 0)
          0
        else if (margin.feeMarket == tradingBaseMarket)
          margin.fee
        else if (margin.feeMarket == margin.toMarket && margin.fromMarket == tradingBaseMarket)
          soldBoughtExchangeRate * margin.fee
        else
          margin.fee * baseCoin.priceInBaseCoin(margin.feeMarket, margin.date)

      def nonZero(x : Double) : Boolean = x.abs >= 1E-7

      def openShort(soldAmount : Double, boughtAmount : Double, feeAmount: Double, feeMarket : Market, feeInTradingBaseCoin : Double) = {
        // Opening a short
        marginSells.add(marketKey, soldAmount, boughtSoldExchangeRate, margin.exchanger, margin.date)

        out.println(
          "%d. ".format(operationNumber) +
            Format.df.format(margin.date) +
            ". Open short " +
            Format.asMarket(soldAmount, soldMarket) + " -> " +
            Format.asMarket(boughtAmount, boughtMarket) +
            ". Fee = " + Format.asMarket(feeAmount, feeMarket) +
            ". " + margin.description
        )
        if (Config.verbosity(Verbosity.showRates))
          out.print(("EXCHANGE RATE:".tab(0) + Format.asMarket(boughtSoldExchangeRate, boughtMarket) + "/" + soldMarket).tab(1))
        if (Config.verbosity(Verbosity.showStocks))
          out.println(marginSells(marketKey))
        out.println()

        if(feeInTradingBaseCoin>0) {
          val fee = Fee(margin.date, margin.id, feeInTradingBaseCoin, tradingBaseMarket, margin.exchanger, margin.description + " fee")
          processFee(fee)
        }
      }

      def openLong(soldAmount : Double, boughtAmount : Double, feeAmount: Double, feeMarket : Market, feeInTradingBaseCoin : Double) = {
        // Opening a long
        marginBuys.add(marketKey, boughtAmount, soldBoughtExchangeRate, margin.exchanger, margin.date)

        out.println(
          "%d. ".format(operationNumber)+
            Format.df.format(margin.date)+
            ". Open long "+
            Format.asMarket(soldAmount, soldMarket)+" -> "+
            Format.asMarket(boughtAmount, boughtMarket)+
            ". Fee = "+Format.asMarket(feeAmount, feeMarket)+
            ". "+margin.description
        )
        if(Config.verbosity(Verbosity.showRates))
          out.println(("EXCHANGE RATE:".tab(0)+Format.asMarket(soldBoughtExchangeRate,soldMarket)+"/"+boughtMarket).tab(1))
        if(Config.verbosity(Verbosity.showStocks))
          out.println(marginBuys(marketKey))
        out.println()

        if(feeInTradingBaseCoin>0) {
          val fee = Fee(margin.date, margin.id, feeInTradingBaseCoin, tradingBaseMarket, margin.exchanger, margin.description + " fee")
          processFee(fee)
        }
      }

      if(margin.orderType == Operation.OrderType.Sell) {
        val inLongsAmount = marginBuys(marketKey).totalAmount

        if(inLongsAmount<=0) {
          // Opening a short
          openShort(soldAmount, boughtAmount, margin.fee, margin.feeMarket, feeInTradingBaseCoin)
        } else {
          // Closing a long
          val closedSoldAmount = soldAmount min inLongsAmount
          val closedBoughtAmount = closedSoldAmount * boughtAmount / soldAmount

          out.println(
            "%d. ".format(operationNumber)+
            Format.df.format(margin.date)+
            ". Close long "+
            Format.asMarket(closedSoldAmount, soldMarket)+" -> "+
            Format.asMarket(closedBoughtAmount, boughtMarket)+
            ". Fee = "+Format.asMarket(margin.fee, margin.feeMarket)+
            ". "+margin.description
          )
          if(Config.verbosity(Verbosity.showRates))
            out.print(("EXCHANGE RATE:".tab(0)+Format.asMarket(boughtSoldExchangeRate,boughtMarket)+"/"+soldMarket).tab(1))
          if(Config.verbosity(Verbosity.showStocks))
            out.println(marginBuys(marketKey))
          else
            out.println()
          val (basis, noBasis) = marginBuys.remove(marketKey, soldAmount)
          if(Config.verbosity(Verbosity.showStocks)) {
            out.print("".tab(1))
            out.println(marginBuys(marketKey))
          }
          out.println()

          val gain = closedBoughtAmount - basis

          if(gain>0)
            processGain(Gain(margin.date, margin.id, gain, tradingBaseMarket, feeInTradingBaseCoin, tradingBaseMarket, margin.exchanger, margin.description + " closed long"))
          else
            processLoss(Loss(margin.date, margin.id, gain.abs, tradingBaseMarket, feeInTradingBaseCoin, tradingBaseMarket, margin.exchanger, margin.description + " closed long"))

          if(nonZero(noBasis)) {
            // we are also opening a short
            val longedSoldAmount = soldAmount - closedSoldAmount // same as noBasis
            val longedBoughtAmount = boughtAmount - closedBoughtAmount
            openShort(longedSoldAmount, longedBoughtAmount, 0, margin.feeMarket, 0)
            // Logger.warning("Shorting No basis > 0: " + (noBasis, longedSoldAmount) + " " + margin)
          }
        }
      } else if(margin.orderType == Operation.OrderType.Buy) {
        val inShortsAmount = marginSells(marketKey).totalAmount

        if(inShortsAmount<=0) {
          // Opening a long
          openLong(soldAmount, boughtAmount, margin.fee, margin.feeMarket, feeInTradingBaseCoin)
        } else {
          // Closing a short
          val closedBoughtAmount = boughtAmount min inShortsAmount
          val closedSoldAmount = closedBoughtAmount * soldAmount / boughtAmount

          out.println(
            "%d. ".format(operationNumber)+
            Format.df.format(margin.date)+
            ". Close short "+
            Format.asMarket(closedSoldAmount, soldMarket)+" -> "+
            Format.asMarket(closedBoughtAmount, boughtMarket)+
            ". Fee = "+Format.asMarket(margin.fee, margin.feeMarket)+
            ". "+margin.description
          )
          if(Config.verbosity(Verbosity.showRates))
            out.print(("EXCHANGE RATE:".tab(0)+Format.asMarket(soldBoughtExchangeRate,soldMarket)+"/"+boughtMarket).tab(1))
          if(Config.verbosity(Verbosity.showStocks))
            out.println(marginSells(marketKey))
          else
            out.println()
          val (basis, noBasis) = marginSells.remove(marketKey, boughtAmount)
          if(Config.verbosity(Verbosity.showStocks)) {
            out.print("".tab(1))
            out.println(marginSells(marketKey))
          }
          out.println()

          val gain = basis - (closedSoldAmount-feeInTradingBaseCoin) - feeInTradingBaseCoin
          if(gain>0)
            processGain(Gain(margin.date, margin.id, gain, tradingBaseMarket, feeInTradingBaseCoin, tradingBaseMarket, margin.exchanger, margin.description + " closed short"))
          else
            processLoss(Loss(margin.date, margin.id, gain.abs, tradingBaseMarket, feeInTradingBaseCoin, tradingBaseMarket, margin.exchanger, margin.description + " closed short"))

          if(nonZero(noBasis)) {
            // we are also opening a long
            val longedSoldAmount = soldAmount - closedSoldAmount
            val longedBoughtAmount = boughtAmount - closedBoughtAmount // same as noBasis
            openLong(longedSoldAmount, longedBoughtAmount, 0, margin.feeMarket, 0)
            // Logger.warning("Longing No basis > 0: " + (noBasis, longedBoughtAmount) + " " + margin)
          }
        }
      }
    }



    def dispatch(operation: Operation): Unit = {
      operation match {
        case exchange: Exchange =>
          processExchange(exchange)

        case loss : Loss =>
          processLoss(loss)

        case gain : Gain =>
          processGain(gain)

        case fee : Fee =>
          processFee(fee)

        case settlement : SettlementBuy =>
          processSettlement(settlement)

        case margin : Margin =>
          processMargin(margin)

        case operations : Operations =>
          operations.operations.foreach(dispatch)
      }
    }


    val startDate = Date(2000,1,1)
    val endDate = Date(2500,12,31)
    var currentYear : Int = operations.head.date.getYear
    initializeYear(currentYear)

    for (operation <- operations if operation.date >= startDate && operation.date <= endDate) {
      val newYear = operation.date.getYear
      if(newYear > currentYear) {
        reportYear(currentYear)
        currentYear = initializeYear(newYear)
      }

      operationNumber += 1
      Logger.trace(operationNumber+" "+operation)

      dispatch(operation)

    }
    reportYear(currentYear)
    out.close()
    csv.close()

    Logger.trace("Output generated in file "+outFile+".")
  }
}