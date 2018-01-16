package taxes

import java.io.PrintStream
import java.text.SimpleDateFormat

import taxes.Exchanger._
import taxes.Market.Market
import taxes.Util.Logger


object Format {
  val header : String = "".padTo(80,'=')
  
  val df = new SimpleDateFormat("yyyy-MM-dd hh:mm:ss")

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
  def process(): Unit = {
    val config = Config.config

    val operations = Exchangers.readAllOperations(config.user).sortBy(_.date)

    if(operations.isEmpty)
      Logger.fatal("No operation was found in any exchange for user: "+config.user+".")

    val outFile = Paths.userOutputFolder+"/output.txt"
    val out = new java.io.PrintStream(outFile)

    // what market is our basis
    val baseCoin = config.baseCoin
    val baseMarket = baseCoin.market

    // current stock of assets with their cost bases (expressed in base coin)
    val stocks = StockQueuePool(baseMarket)

    // current in longs/shorts assets for margin trading operations
    val marginBuys = StockQueuePool(Market.bitcoin)
    val marginSells = StockQueuePool(Market.bitcoin)


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
      return year
    }


    def printQueue(label : String, queue : StockQueue): Unit = {
      val amount = queue.iterator.map(_.amount).sum
      val totalCost = queue.iterator.map(p => p.amount * p.costBasis).sum

      out.print(
        Format.leftPad(label, 5, ' ')+
        Format.leftPad("%.6f".format(amount), 20, ' ')+
        Format.leftPad(Format.asMarket(totalCost, queue.baseMarket), 20, ' ')+
        "  "
      )

      var xs = List[String]()
      for(s <- queue)
        xs ::=
          "(%.6f, ".format(s.amount)+
          Format.asMarket(s.costBasis, queue.baseMarket)+
          ", %s, ".format(s.exchanger)+
          new SimpleDateFormat("yyyy-MM-dd").format(s.date)+
          ")"
       out.println(xs.mkString(", "))
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
          printQueue(queue.market, queue)
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
        for (q <- marginBuys)
          out.println(q._1, q._2)

        out.println("Opened margin shorts:")
        for (q <- marginSells)
          out.println(q._1, q._2)
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
        if (exchange.feeMarket == boughtMarket)
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
        else if (exchange.feeMarket == exchange.toMarket && exchange.fromMarket == baseMarket)
          soldBoughtExchangeRate * exchange.fee
        else
          exchange.fee * baseCoin.priceInBaseCoin(exchange.feeMarket, exchange.date)

      Realized.perMarketPaidFees.record(exchange.feeMarket, feeInBaseCoin)

      // Total value involved in this operation, expressed in base coin
      val totalInBaseCoin =
        if (soldMarket == baseMarket)
          soldAmount
        else if (boughtMarket == baseMarket)
          boughtAmount + feeInBaseCoin
        else if (Market.priority(soldMarket) > Market.priority(boughtMarket))
          soldAmount * baseCoin.priceInBaseCoin(soldMarket, exchange.date)
        else
          boughtAmount * baseCoin.priceInBaseCoin(boughtMarket, exchange.date) + feeInBaseCoin // bought amount does not include fee

      // Price we sold released coins at, expressed in base coin
      val soldPriceInBaseCoin = totalInBaseCoin / soldAmount

      // Price we bought acquired coins at, expressed in base coin
      val boughtBasisPriceInBaseCoin = (totalInBaseCoin - feeInBaseCoin) / boughtAmount

      // Add bought coins with their cost basis to our stock of coins
      if (!boughtBasisPriceInBaseCoin.isNaN() && boughtAmount > 0) {
        stocks.add(boughtMarket, boughtAmount, boughtBasisPriceInBaseCoin, exchange.exchanger, exchange.date)
      }

      // Get cost basis for sold coins from current stocks
      val (soldBasisInBaseCoin, noBasis) = stocks.remove(soldMarket, soldAmount)

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
        Realized.costBases.record(soldMarket, feeInBaseCoin)

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
        if(Config.verbosity(Verbosity.showRates))
          out.println(
            "EXCHANGE RATES: "+Format.asMarket(boughtSoldExchangeRate,boughtMarket)+
            "/%s   ".format (soldMarket)+
            Format.asMarket(soldBoughtExchangeRate,soldMarket)+"/%s".format(boughtMarket)
          )
        if(Config.verbosity(Verbosity.showDetails))
          out.print(
            "SOLD:           "+Format.asMarket(soldAmount, soldMarket)+
            " at "+Format.asMarket(soldPriceInBaseCoin, baseMarket)+
            "/%s".format(soldMarket)+
            " = "+Format.asMarket(sellValueInBaseCoin, baseMarket)
          )
        if(Config.verbosity(Verbosity.showStocks)) {
          out.print("     ")
          printQueue(soldMarket, qSold)
        } else if(Config.verbosity(Verbosity.showDetails))
          out.println()
        if(Config.verbosity(Verbosity.showMoreDetails))
          out.println(
            "COST BASIS:     "+Format.asMarket(soldBasisInBaseCoin,baseMarket)
          )
        if(Config.verbosity(Verbosity.showDetails))
          if(!boughtBasisPriceInBaseCoin.isNaN()) {
            out.print(
              "BOUGHT:         " + Format.asMarket(boughtAmount, boughtMarket) +
              " at " + Format.asMarket(boughtBasisPriceInBaseCoin, baseMarket) +
              "/%s".format(boughtMarket)
            )
            if(Config.verbosity(Verbosity.showStocks)) {
              out.print("     ")
              printQueue(boughtMarket, qBought)
            } else
              out.println()
          }
        if(Config.verbosity(Verbosity.showMoreDetails))
          out.println(
            "FEE:            "+Format.asMarket(exchange.fee, exchange.feeMarket)+
            " = "+Format.asMarket(feeInBaseCoin,baseMarket)
          )
        out.print(if(gainInBaseCoin>0) "GAIN:           " else "LOSS:           ")
        if(Config.verbosity(Verbosity.showDetails))
          out.print(
            Format.asMarket(sellValueInBaseCoin,baseMarket)+
              " - " + Format.asMarket(soldBasisInBaseCoin,baseMarket)+
              " - " + Format.asMarket(feeInBaseCoin,baseMarket)+
              " = "
          )
        out.println(Format.asMarket(gainInBaseCoin,baseMarket))
        out.println()
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
          "PAID FEE:       " + Format.asMarket(fee.amount, fee.market)
        )
        if(Config.verbosity(Verbosity.showStocks)) {
          out.print("     ")
          printQueue(fee.market, stocks(fee.market))
        } else
          out.println()
        out.println(
          "FEE COST BASIS: " + Format.asMarket(feeCostInBaseCoin, baseMarket)
        )
      }
      out.print(
        "FEE:           "+Format.asMarket(-feeCostInBaseCoin, baseMarket)
      )
      if(Config.verbosity(Verbosity.showStocks)) {
        out.print("     ")
        printQueue(fee.market, stocks(fee.market))
      } else
        out.println()
      out.println()
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
        out.print(
          "PAID:           " + Format.asMarket(loss.amount, loss.market)
        )
        if(Config.verbosity(Verbosity.showStocks)) {
          out.print("     ")
          printQueue(loss.market, stocks(loss.market))
        } else
          out.println()
        out.println(
          "COST BASIS:     " + Format.asMarket(basisInBaseCoin, baseMarket)
        )
      }
      if(Config.verbosity(Verbosity.showMoreDetails)) {
        out.print(
          "PAID FEE:       " + Format.asMarket(loss.fee, loss.feeMarket)
        )
        if(Config.verbosity(Verbosity.showStocks)) {
          out.print("     ")
          printQueue(loss.feeMarket, stocks(loss.feeMarket))
        } else if(Config.verbosity(Verbosity.showMoreDetails))
          out.println()
        out.println(
          "FEE COST BASIS: " + Format.asMarket(feeBasisInBaseCoin, baseMarket)
        )
      }

      if(feeBasisInBaseCoin>0)
        out.println(
          "FEE:            " + Format.asMarket(-feeBasisInBaseCoin, baseMarket)
        )
      out.print(
        "LOSS:           " + Format.asMarket(-basisInBaseCoin, baseMarket)
      )
      if(Config.verbosity(Verbosity.showStocks)) {
        out.print("     ")
        printQueue(loss.market, stocks(loss.market))
      } else
        out.println()
      out.println()
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
        out.print(
          "RECEIVED:       " + Format.asMarket(gain.amount, gain.market) +
          " at " + Format.asMarket(basePrice, baseMarket) +
          "/%s".format(gain.market) +
          " = " + Format.asMarket(gainInBaseCoin, baseMarket)
        )
        if(Config.verbosity(Verbosity.showStocks)) {
          out.print("     ")
          printQueue(gain.market, stocks(gain.market))
        } else
          out.println()
        out.print(
          "PAID FEE:       " + Format.asMarket(gain.fee, gain.feeMarket)
        )
        if(Config.verbosity(Verbosity.showStocks)) {
          out.print("     ")
          printQueue(gain.feeMarket, stocks(gain.feeMarket))
        } else
          out.println()
        out.println(
          "FEE COST BASIS: " + Format.asMarket(feeBasisInBaseCoin, baseMarket)
        )
      }
      if(feeBasisInBaseCoin>0)
        out.println(
          "FEE:            " + Format.asMarket(-feeBasisInBaseCoin, baseMarket)
        )
      out.print(
        "GAIN:           "
      )
      out.println(
        Format.asMarket(gainInBaseCoin, baseMarket)
      )
      out.println()
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
        out.println(
          "EXCHANGE RATES: "+Format.asMarket(boughtSoldExchangeRate,boughtMarket)+
          "/%s   ".format (soldMarket)+
          Format.asMarket(soldBoughtExchangeRate,soldMarket)+"/%s".format(boughtMarket)
        )

      // This is like a loss but we have to remove bought coins from stock of opened shorts
      if(marginSells(marketKey).nonEmpty) {
        if(Config.verbosity(Verbosity.showStocks))
          printQueue(marketKey, marginSells(marketKey))

        marginSells(marketKey).removeAndGetBasis(boughtAmount)

        if(Config.verbosity(Verbosity.showStocks))
          printQueue(marketKey, marginSells(marketKey))
      }
      out.println()

      processLoss(Loss(settlement.date, settlement.id, totalInTradingBaseCoin, tradingBaseMarket, feeInTradingBaseCoin, tradingBaseMarket, settlement.exchanger, settlement.description))
    }


    def processMargin(margin: Margin) {
      val soldMarket = margin.fromMarket
      val soldAmount = margin.fromAmount

      val boughtMarket = margin.toMarket
      val boughtAmount = margin.toAmount

      val (tradedMarket, tradingBaseMarket) = margin.pair

      val marketKey = marginPairKey(tradedMarket, tradingBaseMarket)

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

      if(margin.orderType == Operation.OrderType.Sell) {
        if(marginBuys(marketKey).isEmpty) {
          // Opening a short
          marginSells.add(marketKey, soldAmount, boughtSoldExchangeRate, margin.exchanger, margin.date)

          out.println(
            "%d. ".format(operationNumber)+
            Format.df.format(margin.date)+
            ". Open short "+
            Format.asMarket(soldAmount, soldMarket)+" -> "+
            Format.asMarket(boughtAmount, boughtMarket)+
            ". Fee = "+Format.asMarket(margin.fee, margin.feeMarket)+
            ". "+margin.description
          )
          if(Config.verbosity(Verbosity.showRates))
            out.println("EXCHANGE RATE:  "+Format.asMarket(boughtSoldExchangeRate,boughtMarket)+"/"+soldMarket)
          if(Config.verbosity(Verbosity.showStocks))
            printQueue(marketKey, marginSells(marketKey))
          out.println()

          val fee = Fee(margin.date, margin.id, feeInTradingBaseCoin, tradingBaseMarket, margin.exchanger, margin.description+" fee" )
          processFee(fee)
        }  else {
          // Closing a long
          out.println(
            "%d. ".format(operationNumber)+
            Format.df.format(margin.date)+
            ". Close long "+
            Format.asMarket(soldAmount, soldMarket)+" -> "+
            Format.asMarket(boughtAmount, boughtMarket)+
            ". Fee = "+Format.asMarket(margin.fee, margin.feeMarket)+
            ". "+margin.description
          )
          if(Config.verbosity(Verbosity.showRates))
            out.println("EXCHANGE RATE:  "+Format.asMarket(boughtSoldExchangeRate,boughtMarket)+"/"+soldMarket)
          if(Config.verbosity(Verbosity.showStocks))
            printQueue(marketKey, marginBuys(marketKey))
          val (basis, noBasis) = marginBuys.remove(marketKey, soldAmount)
          if(Config.verbosity(Verbosity.showStocks))
            printQueue(marketKey, marginBuys(marketKey))
          out.println()

          val gain = boughtAmount - basis

          if(gain>0)
            processGain(Gain(margin.date, margin.id, gain, tradingBaseMarket, feeInTradingBaseCoin, tradingBaseMarket, margin.exchanger, margin.description + " closed long"))
          else
            processLoss(Loss(margin.date, margin.id, gain.abs, tradingBaseMarket, feeInTradingBaseCoin, tradingBaseMarket, margin.exchanger, margin.description + " closed long"))

          // toDo if noBasis > 0 we are also opening a short
          if(noBasis>1E-7)
            Logger.warning("No basis > 0: "+noBasis+" "+margin)
        }
      } else if(margin.orderType == Operation.OrderType.Buy) {
        if(marginSells(marketKey).isEmpty) {
          // Opening a long
          marginBuys.add(marketKey, boughtAmount, soldBoughtExchangeRate, margin.exchanger, margin.date)

          out.println(
            "%d. ".format(operationNumber)+
            Format.df.format(margin.date)+
            ". Open long "+
            Format.asMarket(soldAmount, soldMarket)+" -> "+
            Format.asMarket(boughtAmount, boughtMarket)+
            ". Fee = "+Format.asMarket(margin.fee, margin.feeMarket)+
            ". "+margin.description
          )
          if(Config.verbosity(Verbosity.showRates))
            out.println("EXCHANGE RATE:  "+Format.asMarket(soldBoughtExchangeRate,soldMarket)+"/"+boughtMarket)
          if(Config.verbosity(Verbosity.showStocks))
            printQueue(marketKey, marginBuys(marketKey))
          out.println()

          val fee = Fee(margin.date, margin.id, feeInTradingBaseCoin, tradingBaseMarket, margin.exchanger, margin.description+" fee" )
          processFee(fee)
        } else {
          // Closing a short
          out.println(
            "%d. ".format(operationNumber)+
            Format.df.format(margin.date)+
            ". Close short "+
            Format.asMarket(soldAmount, soldMarket)+" -> "+
            Format.asMarket(boughtAmount, boughtMarket)+
            ". Fee = "+Format.asMarket(margin.fee, margin.feeMarket)+
            ". "+margin.description
          )
          if(Config.verbosity(Verbosity.showRates))
            out.println("EXCHANGE RATE:  "+Format.asMarket(soldBoughtExchangeRate,soldMarket)+"/"+boughtMarket)
          if(Config.verbosity(Verbosity.showStocks))
            printQueue(marketKey, marginSells(marketKey))
          val (basis, noBasis) = marginSells.remove(marketKey, boughtAmount)
          if(Config.verbosity(Verbosity.showStocks))
            printQueue(marketKey, marginSells(marketKey))
          out.println()

          val gain = basis - (soldAmount-feeInTradingBaseCoin) - feeInTradingBaseCoin
          if(gain>0)
            processGain(Gain(margin.date, margin.id, gain, tradingBaseMarket, feeInTradingBaseCoin, tradingBaseMarket, margin.exchanger, margin.description + " closed short"))
          else
            processLoss(Loss(margin.date, margin.id, gain.abs, tradingBaseMarket, feeInTradingBaseCoin, tradingBaseMarket, margin.exchanger, margin.description + " closed short"))

          // toDo if noBasis > 0 we are also opening a long
          if(noBasis>1E-7)
            Logger.warning("No basis > 0: "+noBasis+" "+margin)
        }
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
      }
    }
    reportYear(currentYear)
    out.close()

    Logger.trace("Output generated in file "+outFile+".")
  }
}
