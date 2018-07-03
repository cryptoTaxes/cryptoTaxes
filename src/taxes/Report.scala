package taxes

import java.io.PrintStream

import taxes.Exchanger._
import taxes.HTMLDoc.HTML
import taxes.Market.Market
import taxes.Util.Logger

import scala.collection.mutable.ListBuffer


case class ValueTracker(baseMarket : Market) extends Iterable[(Market, Double)] with ToHTML {
  private val map = scala.collection.mutable.Map[Market,Double]()

  def clear(): Unit =
    map.clear()

  def record(market : Market, amount : Double): Unit =
    map += (market -> (amount + map.getOrElse(market, 0.0)))

  def sum : Double =
    map.values.sum

  def keys : Iterable[Market] =
    map.keys

  def iterator : Iterator[(Market, Double)] =
    map.iterator

  def apply(market : Market) : Double =
    map.getOrElse(market, 0.0)

  override def toHTML : HTML =
    toHTML()

  def toHTML(caption : String = "") : HTML =
    <table id='tableStyle1'>
      <tr>
        <th>Market</th>
        <th>Amount</th>
        <th>Accumulated</th>
      </tr>
      {if(caption.nonEmpty)
        <caption>{caption}</caption>
      }
      {var sum = 0.0
       map.toList.sortBy(_._2).map { case (label,total) =>
         sum += total
         <tr>
           <td ><span class='market'>{label}</span></td>
           <td>{HTMLDoc.asMarket(total, baseMarket)}</td>
           <td>{HTMLDoc.asMarket(sum, baseMarket)}</td>
        </tr>
      }
      }
      <td class='embold'>Total:</td>
      <td></td>
      <td>{HTMLDoc.asMarket(map.values.sum, baseMarket)}</td>
    </table>
}


object OperationTracker {
  case class CSVEntry(
    date : Date
    , exchanger : Exchanger
    , description : String
    , costBasis : Double
    , proceeds : Double
    , fee : Double
    )
  private val emptyEntry = OperationTracker.CSVEntry(new Date(0,0,0), new General(""), "", 0, 0, 0)
}


case class OperationTracker() extends Iterable[(Int,OperationTracker.CSVEntry)] {
  private val m = scala.collection.mutable.Map[Int, OperationTracker.CSVEntry]()

  override def iterator: Iterator[(Int,OperationTracker.CSVEntry)] =
    m.iterator.toList.sortBy(_._1).toIterator

  def recordCostBasis(operationNumber : Int, amount : Double): Unit = {
    val entry = m.getOrElse(operationNumber, OperationTracker.emptyEntry)
    m(operationNumber) = entry.copy(costBasis = entry.costBasis + amount)
  }

  def recordProceeds(operationNumber : Int, amount : Double): Unit = {
    val entry = m.getOrElse(operationNumber, OperationTracker.emptyEntry)
    m(operationNumber) = entry.copy(proceeds = entry.proceeds + amount)
  }

  def recordFee(operationNumber : Int, amount : Double): Unit = {
    val entry = m.getOrElse(operationNumber, OperationTracker.emptyEntry)
    m(operationNumber) = entry.copy(fee = entry.fee + amount)
  }

  def setDate(operationNumber : Int, date : Date): Unit = {
    val entry = m.getOrElse(operationNumber, OperationTracker.emptyEntry)
    m(operationNumber) = entry.copy(date = date)
  }

  def setDescription(operationNumber : Int, description : String): Unit = {
    val entry = m.getOrElse(operationNumber, OperationTracker.emptyEntry)
    m(operationNumber) = entry.copy(description = description)
  }

  def setExchanger(operationNumber : Int, exchanger : Exchanger): Unit = {
    val entry = m.getOrElse(operationNumber, OperationTracker.emptyEntry)
    m(operationNumber) = entry.copy(exchanger = exchanger)
  }

  def clear(): Unit =
    m.clear()

  def printToCSVFile(fileName : String, year : Int): Unit = {
    val csvFileName = Paths.userOutputFolder+"/%s%d.csv".format(fileName,year)
    val ps = new PrintStream(csvFileName)

    ps.println()
    ps.println("%s %d".format(Accounting.toString(Config.config.accountingMethod), year))
    ps.println("")

    val sep = ";"
    val header = List("order", "date", "exchanger", "description", "cost basis/loss", "proceeds/gain", "fee")

    ps.println(header.mkString(sep))
    for((operationNumber,entry) <- this)
      ps.println(List[Any](operationNumber, Format.shortDf.format(entry.date), entry.exchanger, entry.description, entry.costBasis, entry.proceeds, entry.fee).mkString(sep))

    ps.close()
  }
}


object Report {
  // All quantities expressed in base coin
  trait Realized {
    // record realized gains/losses/paid fees per market
    val perMarketGains : ValueTracker
    val perMarketLooses : ValueTracker
    val perMarketPaidFees : ValueTracker // fees are not already deducted from gains and added to looses

    // Difference between these minus fees is realized gains
    val costBasis : ValueTracker  // cost bases of sold coins
    val proceeds : ValueTracker // value obtained for sold coins
  }

  def process(): Unit = {
    val config = Config.config

    val operations = Exchangers.readAllOperations().sortBy(_.date)

    if(operations.isEmpty)
      Logger.fatal("No operation was found in any exchange for user: "+config.user+".")

    val processedOperations = ListBuffer[Processed]()


    // what market is our basis
    val baseCoin = config.baseCoin
    val baseMarket = baseCoin.market

    // current stock of assets with their cost bases (expressed in base coin)
    val accountingMethod = Config.config.accountingMethod
    val stocks =
      if(accountingMethod == Accounting.FIFO)
        QueueStockPool(baseMarket)
      else if (accountingMethod == Accounting.LIFO)
        StackStockPool(baseMarket)
      else
        Logger.fatal("Unkown accounting method: %s".format(Accounting.toString(accountingMethod)))

    // current in longs/shorts assets for margin trading operations
    val marginBuysMap = scala.collection.immutable.Map[Exchanger, StockPool](
        Poloniex -> QueueStockPool(Market.bitcoin)
      , Bitfinex -> StackStockPool(Market.usd)
      )
    val marginSellsMap = scala.collection.immutable.Map[Exchanger, StockPool](
        Poloniex -> QueueStockPool(Market.bitcoin)
      , Bitfinex -> StackStockPool(Market.usd)
      )

    // Sold stocks without corresponding buys
    val partiallyFrees = ListBuffer[String]()
    val frees = ListBuffer[Exchange]()

    // All quantities expressed in base coin
    object Realized extends Realized {
      // record realized gains/losses/paid fees per market
      val perMarketGains = ValueTracker(baseMarket)
      val perMarketLooses = ValueTracker(baseMarket)
      val perMarketPaidFees = ValueTracker(baseMarket) // fees are not already deducted from gains and added to looses

      // Difference between these minus fees is realized gains
      val costBasis = ValueTracker(baseMarket)  // cost bases of sold coins
      val proceeds = ValueTracker(baseMarket) // value obtained for sold coins
    }

    // order of current operation within current year
    var operationNumber = 0

    val operationTracker = OperationTracker()

    def initializeYear(year : Int): Int = {
      Realized.perMarketGains.clear()
      Realized.perMarketLooses.clear()
      Realized.perMarketPaidFees.clear()

      Realized.costBasis.clear()
      Realized.proceeds.clear()

      partiallyFrees.clear()
      frees.clear()

      operationNumber = 0

      processedOperations.clear()

      operationTracker.clear()
      return year
    }


    def reportYear(year : Int): Unit = {
      val method = Accounting.toString(Config.config.accountingMethod)

      val htmlReportFile = Paths.userOutputFolder+"/%s%d.html".format(method, year)
      val htmlReportTitle = "%d %s Report".format(year, method)
      val htmlReport = HTMLDoc(htmlReportFile, htmlReportTitle)

      htmlReport += <div class='header'>{htmlReportTitle}</div>

      for(processed <- processedOperations)
        htmlReport += processed

      htmlReport += HTMLDoc.reportResults(year, Realized)
      htmlReport.close()


      val htmlPortfolioFile = Paths.userOutputFolder+"/Portfolio%d.html".format(year)
      val htmlPortfolioTitle = "%d End of year portfolio".format(year)
      val htmlPortfolio = HTMLDoc(htmlPortfolioFile, htmlPortfolioTitle)

      htmlPortfolio += stocks.toHTML(htmlPortfolioTitle)
      htmlPortfolio.close()


      val htmlExtraFile = Paths.userOutputFolder+"/Extra%d.html".format(year)
      val htmlExtraTitle = "%d Statistics".format(year)
      val htmlExtra = HTMLDoc(htmlExtraFile, htmlExtraTitle)

      htmlExtra += <div class='header'>{htmlExtraTitle}</div>
      htmlExtra += HTMLDoc.reportYear(year, Realized)

      if(Config.verbosity(Verbosity.showMoreDetails)) {
        {htmlExtra += <div>Frees:</div>}
        <div>
        {for(f <- partiallyFrees)
          htmlExtra += <div>{f}</div>
        }
        </div>

        {htmlExtra += <div>Priced0:</div>}
        <div>
          {for(op <- frees)
            htmlExtra += <div>{op}</div>
          }
        </div>

        {htmlExtra += <div>Opened margin longs:</div>}
        <div>
          {for(marginBuy <- marginBuysMap.values)
            for (cont <- marginBuy)
              if(cont.totalAmount>0)
                htmlExtra += <div>{cont.toHTML(showTotal = true)}</div>
          }
        </div>

        {htmlExtra += <div>Opened margin shorts:</div>}
        <div>
          {for(marginSell <- marginSellsMap.values)
            for (cont <- marginSell)
             if(cont.totalAmount>0)
                htmlExtra += <div>{cont.toHTML(showTotal = true)}</div>
          }
        </div>
      }
      htmlExtra.close()

      operationTracker.printToCSVFile("FIFO", year)
    }

    def marginPairKey(market1 : Market, market2: Market) : String
    = market1 ++ "/" ++ market2

    def preprocessExchange(exchange: Exchange) : Processed.Composed = {
      val soldMarket = exchange.fromMarket
      val soldAmount = exchange.fromAmount // without fee

      val boughtMarket = exchange.toMarket
      val boughtAmount = exchange.toAmount // without fee

      val feeMarket = exchange.feeMarket
      // if fee is explicitly market as detached or if it is not
      // expressed in any of two markets involved in this exchange
      // then detach if from this exchange
      val isDetachedFee = exchange.isDetachedFee || (feeMarket != soldMarket && feeMarket != boughtMarket)
      val feeAmount = // only if fee is included in this exchange
        if(isDetachedFee)
          0
        else
          exchange.feeAmount

      val (totalSoldAmount, totalBoughtAmount) = // including fee
        if (feeAmount == 0)
          (soldAmount, boughtAmount)
        else if(feeMarket == soldMarket)
          (soldAmount + feeAmount, boughtAmount)
        else if(feeMarket == boughtMarket)
          (soldAmount, boughtAmount + feeAmount)
        else
          Logger.fatal("Cannot process exchange as fee is not expressed in same unit as from or to markets." + exchange.toString)


      if(totalSoldAmount==0)
        Logger.warning("Warning: no sold coins in tis exchange %s.".format(exchange))
      if(totalBoughtAmount==0)
        Logger.warning("Warning: no bought coins in this exchange %s.".format(exchange))


      // Compute exchange rates (fee is included)
      val (boughtSoldExchangeRate, soldBoughtExchangeRate) =
          (totalBoughtAmount / totalSoldAmount, totalSoldAmount / totalBoughtAmount)

      // Total value involved in this operation, expressed in base coin
      // and proxy used to compute (base coin expressed) prices
      val (totalInBaseCoin, baseCoinProxy, baseCoinProxyRate) =
        if (soldMarket == baseMarket)
          (totalSoldAmount, baseMarket, 1.0)
        else if (boughtMarket == baseMarket)
          (totalBoughtAmount, baseMarket, 1.0)
        else if (Market.priority(soldMarket) > Market.priority(boughtMarket)) {
          val rate = baseCoin.priceInBaseCoin(soldMarket, exchange.date)
          (totalSoldAmount * rate, soldMarket, rate)
        } else {
          val rate = baseCoin.priceInBaseCoin(boughtMarket, exchange.date)
          (totalBoughtAmount * rate, boughtMarket, rate)
        }

      // fee for this operation, expressed in base coin,
      // only if fee is included in this exchange
      val feeInBaseCoin =
        if (isDetachedFee || feeAmount == 0)
          0
        else if (exchange.feeMarket == baseMarket)
          feeAmount
        else if (exchange.feeMarket == boughtMarket && soldMarket == baseMarket)
          soldBoughtExchangeRate * feeAmount
        else if (exchange.feeMarket == soldMarket && boughtMarket == baseMarket)
          boughtSoldExchangeRate * feeAmount
        else if (exchange.feeMarket == boughtMarket)
          feeAmount * totalInBaseCoin / totalBoughtAmount
        else if (exchange.feeMarket == soldMarket)
          feeAmount * totalInBaseCoin / totalSoldAmount
        else
          Logger.fatal("Cannot process exchange as fee is not expressed in same unit as from or to markets." + exchange.toString)

      // Price we sold released coins at, expressed in base coin
      val soldPriceInBaseCoin = totalInBaseCoin / totalSoldAmount

      // Price we bought acquired coins at, expressed in base coin
      val boughtBasisPriceInBaseCoin = totalInBaseCoin / totalBoughtAmount

      // Add bought coins (without paid fee) with their cost basis to our stock of assets
      if (!boughtBasisPriceInBaseCoin.isNaN && boughtAmount > 0) {
        stocks.add(boughtMarket, boughtAmount, boughtBasisPriceInBaseCoin, exchange.exchanger, exchange.date, soldBoughtExchangeRate, soldMarket)
      }

      // Get cost basis for total sold coins from current stock
      val (totalSoldBasisInBaseCoin, noBasis, usedStocks) =
        if (soldMarket != baseMarket)
          stocks.remove(soldMarket, totalSoldAmount)
        else {
          // If our base coin is Euro, cost basis is the amount of Euros involved
          val (_,_,usedStocks) = stocks.remove(soldMarket, totalSoldAmount)
          (totalSoldAmount, 0.0, usedStocks)
        }

      // Value of sold coins (without fee), expressed in base coin
      val proceedsInBaseCoin =
        if(soldAmount==0)
          0
        else
          soldPriceInBaseCoin * soldAmount

      // Cost basis of sold coins (without fee), expressed in base coin
      val soldBasisInBaseCoin =
        if(totalSoldAmount==0)
          0
        else
          totalSoldBasisInBaseCoin * soldAmount / totalSoldAmount

      // Gain in this exchange (without fee), expressed in base coin
      val gainInBaseCoin =
        if (soldMarket == baseMarket) // If base coin is Euros, releasing Euros makes no profit
          0
        else
          proceedsInBaseCoin - soldBasisInBaseCoin

      // Update paid fees
      if(!isDetachedFee)
        Realized.perMarketPaidFees.record(exchange.feeMarket, feeInBaseCoin)

      // Update total gains for soldMarket
      if (gainInBaseCoin > 0)
        Realized.perMarketGains.record(soldMarket, gainInBaseCoin)
      else if (gainInBaseCoin < 0)
        Realized.perMarketLooses.record(soldMarket, gainInBaseCoin.abs)

      // Update realized cost basis and proceeds
      if(soldMarket == baseMarket) {
        // this is the case for Euro -> Any Thing if your base is Euro.
        ;
      } else {
        Realized.costBasis.record(soldMarket, soldBasisInBaseCoin)
        Realized.proceeds.record(soldMarket, proceedsInBaseCoin)
      }

      if (totalSoldBasisInBaseCoin == 0 && (soldMarket != baseMarket))
        frees += exchange // These assets were acquired for free

      if (noBasis.abs > 0.01 && (soldMarket != baseMarket))
        // Part of these assets were acquired for free
        partiallyFrees += "Was SOLD but were partially free %.8f of %.6f %s = %.8f %s  ".format(noBasis, totalSoldAmount, soldMarket, noBasis * proceedsInBaseCoin / soldAmount, baseMarket)

      if(!isDetachedFee)
        operationTracker.recordFee(operationNumber, feeInBaseCoin)

      if(soldMarket == baseMarket) {
        ;
      } else {
        operationTracker.recordCostBasis(operationNumber, soldBasisInBaseCoin)
        operationTracker.recordProceeds(operationNumber, proceedsInBaseCoin)
      }

      val processedExchange =
          Processed.Exchange(
            operationNumber = operationNumber
          , exchange = exchange
          , baseCoinProxy = baseCoinProxy
          , baseCoinProxyRate = baseCoinProxyRate
          , boughtBasisPriceInBaseCoin = boughtBasisPriceInBaseCoin
          , soldPriceInBaseCoin = soldPriceInBaseCoin
          , proceedsInBaseCoin = proceedsInBaseCoin
          , soldBasisInBaseCoin = soldBasisInBaseCoin
          , feeInBaseCoin = feeInBaseCoin
          , gainInBaseCoin = gainInBaseCoin
          , boughtSoldExchangeRate = boughtSoldExchangeRate
          , soldBoughtExchangeRate = soldBoughtExchangeRate
          , usedStocks = usedStocks
          , buys = stocks(boughtMarket).copy
          , sells = stocks(soldMarket).copy
        )

      var processed = List[Processed]()

      // A settlement is bought to pay some dues so this is like a buy followed
      // by a loss but
      if(exchange.isSettlement && exchange.exchanger == Poloniex) {
        val marketKey = marginPairKey(exchange.toMarket, exchange.fromMarket)

        val stockContainer = marginSellsMap(Poloniex)(marketKey)
        if (stockContainer.nonEmpty) {
          stockContainer.removeAndGetBasis(totalBoughtAmount)
          // We bought these to pay for fees and looses of a short that went against us.
          // We need to clear remaining margin sells
        }
        val processedLoss = preprocessLoss(
          Loss(
            date = exchange.date
            , id = exchange.id
            , amount = totalBoughtAmount
            , market = exchange.toMarket
            , exchanger = exchange.exchanger
            , description = ""
            ))
        processed ::= processedLoss
      }

      // If fee is not part of the exchange add it to composed result
      if(isDetachedFee) {
        val processedFee = preprocessFee(
          Fee(
            date = exchange.date
            , id = exchange.id
            , amount = exchange.feeAmount
            , market = exchange.feeMarket
            , exchanger = exchange.exchanger
            , description = ""
            ))
        processed ::= processedFee
      }

      processed ::= processedExchange // order for a Composed is important. Exchange must be at front
      return Processed.Composed(operationNumber, processed)
    }


    def preprocessFee(fee : Fee) : Processed.Fee = {
      val (feeInBaseCoin, _, usedStocks) = stocks.remove(fee.market, fee.amount)

      // Record paid fees
      Realized.perMarketPaidFees.record(fee.market, feeInBaseCoin)

      operationTracker.recordFee(operationNumber, feeInBaseCoin)

      return Processed.Fee(
        operationNumber = operationNumber
        , fee = fee
        , feeInBaseCoin = feeInBaseCoin
        , usedStocks = usedStocks
      )
    }


    def preprocessLoss(loss : Loss) : Processed.Loss = {
      val (lossBasisInBaseCoin, _, usedStocks) = stocks.remove(loss.market, loss.amount)

      // Record looses for loss market
      Realized.perMarketLooses.record(loss.market, lossBasisInBaseCoin)

      // Record cost bases of lost coins
      Realized.costBasis.record(loss.market, lossBasisInBaseCoin)

      operationTracker.recordCostBasis(operationNumber, lossBasisInBaseCoin)

      return Processed.Loss(
        operationNumber = operationNumber
        , loss = loss
        , lossInBaseCoin = lossBasisInBaseCoin
        , usedStocks = usedStocks
      )
    }


    def preprocessGain(gain : Gain) : Processed.Gain = {
      // Record cost basis of gained coins at price corresponding to gain date
      val basePrice = baseCoin.priceInBaseCoin(gain.market, gain.date)
      stocks.add(gain.market, gain.amount, basePrice, gain.exchanger, gain.date, basePrice, baseMarket)

      val gainInBaseCoin = gain.amount * basePrice

      // Update total gains for corresponding market
      Realized.perMarketGains.record(gain.market, gainInBaseCoin)

      // Make the gain accountable now (by considering it as a proceed)
      Realized.proceeds.record(gain.market, gainInBaseCoin)

      operationTracker.recordProceeds(operationNumber, gainInBaseCoin)

      return Processed.Gain(
        operationNumber = operationNumber
        , gain = gain
        , gainInBaseCoin = gainInBaseCoin
        , basePrice = basePrice
      )
    }


    def preprocessMargin(margin: Margin) : Seq[Processed] = {
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

      // Compute exchange rates (without considering fees)
      val (boughtSoldExchangeRate, soldBoughtExchangeRate) =
        if (margin.feeMarket == boughtMarket)
          (boughtAmount / soldAmount, soldAmount / boughtAmount)
        else if (margin.feeMarket == soldMarket)
          (boughtAmount / soldAmount, soldAmount / boughtAmount)
        else
          Logger.fatal("Cannot compute exchange rates as fee is not expressed in same unit as from or to units " + margin.toString)

      // Compute margin exchange rates
      val (marginBoughtSoldExchangeRate, marginSoldBoughtExchangeRate) =
        if (margin.feeMarket == boughtMarket)
          ((boughtAmount - margin.fee) / soldAmount, soldAmount / (boughtAmount - margin.fee))
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
        else if (margin.feeMarket == boughtMarket && soldMarket == tradingBaseMarket)
          soldBoughtExchangeRate * margin.fee
        else if (margin.feeMarket == soldMarket && boughtMarket == tradingBaseMarket)
          boughtSoldExchangeRate * margin.fee
        else
          Logger.fatal("Cannot compute fee as fee is not expressed in same unit as from or to units " + margin.toString)

      var processed = List[Processed]()
      var processedAux = List[Processed]()

      def nonZero(x : Double) : Boolean = x.abs >= 1E-7

      def openShort(soldAmount : Double, boughtAmount : Double, feeAmount: Double, feeMarket : Market, feeInTradingBaseCoin : Double) : List[Processed] = {
        // Opening a short
        marginSells.add(marketKey, soldMarket, soldAmount, marginBoughtSoldExchangeRate, margin.exchanger, margin.date, marginBoughtSoldExchangeRate, boughtMarket)

        var processed = List[Processed]()

        processed ::=
          Processed.Margin(
            operationNumber = operationNumber
            , date = margin.date
            , exchanger = margin.exchanger
            , what = "Open short"
            , fromAmount = soldAmount, fromMarket = soldMarket
            , toAmount = boughtAmount, toMarket = boughtMarket
            , exchangeRate = marginBoughtSoldExchangeRate
            , description = margin.description
            , usedStocksOpt = None
            , marginBuys = marginBuys(marketKey).copy
            , marginSells = marginSells(marketKey).copy
          )

        if(feeInTradingBaseCoin>0) {
          val fee = Fee(margin.date, margin.id, margin.fee, margin.feeMarket, margin.exchanger, "")
          processed ::= preprocessFee(fee)
        }
        return processed
      }

      def openLong(soldAmount : Double, boughtAmount : Double, feeAmount: Double, feeMarket : Market, feeInTradingBaseCoin : Double) : List[Processed] = {
        // Opening a long
        marginBuys.add(marketKey, boughtMarket, boughtAmount, marginSoldBoughtExchangeRate, margin.exchanger, margin.date, marginSoldBoughtExchangeRate, soldMarket)

        var processed = List[Processed]()

        processed ::=
          Processed.Margin(
            operationNumber = operationNumber
            , date = margin.date
            , exchanger = margin.exchanger
            , what = "Open long"
            , fromAmount = soldAmount, fromMarket = soldMarket
            , toAmount = boughtAmount, toMarket = boughtMarket
            , exchangeRate = marginSoldBoughtExchangeRate
            , description = margin.description
            , usedStocksOpt = None
            , marginBuys = marginBuys(marketKey).copy
            , marginSells = marginSells(marketKey).copy
          )

        if(feeInTradingBaseCoin>0) {
          val fee =
            if(margin.exchanger == Poloniex)
              Fee(margin.date, margin.id, feeInTradingBaseCoin, tradingBaseMarket, margin.exchanger, "", Some(feeInTradingBaseCoin*boughtSoldExchangeRate, boughtMarket))
            else
              Fee(margin.date, margin.id, margin.fee, margin.feeMarket, margin.exchanger, "")
          processed ::= preprocessFee(fee)
        }
        return processed
      }

      if(margin.orderType == Operation.OrderType.Sell) {
        val inLongsAmount = marginBuys(marketKey).totalAmount

        if(inLongsAmount<=0) {
          // Opening a short
          processed = openShort(soldAmount, boughtAmount, margin.fee, margin.feeMarket, feeInTradingBaseCoin) ++ processed
        } else {
          // Closing a long
          val closedSoldAmount = soldAmount min inLongsAmount
          val closedBoughtAmount = closedSoldAmount * marginBoughtSoldExchangeRate

          val (basis, noBasis, usedStocks) = marginBuys.remove(marketKey, soldAmount)

          processed ::=
            Processed.Margin(
              operationNumber = operationNumber
              , date = margin.date
              , exchanger = margin.exchanger
              , what = "Close long"
              , fromAmount = closedSoldAmount, fromMarket = soldMarket
              , toAmount = closedBoughtAmount, toMarket = boughtMarket
              , exchangeRate = marginBoughtSoldExchangeRate
              , description = margin.description
              , usedStocksOpt = Some(usedStocks)
              , marginBuys = marginBuys(marketKey).copy
              , marginSells = marginSells(marketKey).copy
            )

          val gain = closedBoughtAmount - basis

          val gainLoss =
            if(gain>0)
              preprocessGain(Gain(margin.date, margin.id, gain, tradingBaseMarket, margin.exchanger, ""))
            else
              preprocessLoss(Loss(margin.date, margin.id, gain.abs, tradingBaseMarket, margin.exchanger, ""))

          if(feeInTradingBaseCoin>0) {
            val fee = Fee(margin.date, margin.id, margin.fee, margin.feeMarket, margin.exchanger, "")
            processed ::= preprocessFee(fee)
          }

          processed ::= gainLoss

          if(nonZero(noBasis)) {
            // we are also opening a short
            val longedSoldAmount = soldAmount - closedSoldAmount // same as noBasis
            val longedBoughtAmount = boughtAmount - closedBoughtAmount
            processedAux = openShort(longedSoldAmount, longedBoughtAmount, 0, margin.feeMarket, 0)
          }
        }
      } else if(margin.orderType == Operation.OrderType.Buy) {
        val inShortsAmount = marginSells(marketKey).totalAmount

        if(inShortsAmount<=0) {
          // Opening a long
          processed = openLong(soldAmount, boughtAmount, margin.fee, margin.feeMarket, feeInTradingBaseCoin) ++ processed
        } else {
          // Closing a short
          val closedBoughtAmount = boughtAmount min inShortsAmount
          val closedSoldAmount = closedBoughtAmount * marginSoldBoughtExchangeRate

          val (basis, noBasis, usedStocks) = marginSells.remove(marketKey, boughtAmount)

          processed ::=
            Processed.Margin(
              operationNumber = operationNumber
              , date = margin.date
              , exchanger = margin.exchanger
              , what = "Close short"
              , fromAmount = closedSoldAmount, fromMarket = soldMarket
              , toAmount = closedBoughtAmount, toMarket = boughtMarket
              , exchangeRate = marginSoldBoughtExchangeRate
              , description = margin.description
              , usedStocksOpt = Some(usedStocks)
              , marginBuys = marginBuys(marketKey).copy
              , marginSells = marginSells(marketKey).copy
            )

          val gain = basis - closedSoldAmount

          val gainLoss =
            if(gain>0)
              preprocessGain(Gain(margin.date, margin.id, gain, tradingBaseMarket, margin.exchanger, "") /*, -closedSoldAmount*/)
            else
              preprocessLoss(Loss(margin.date, margin.id, gain.abs, tradingBaseMarket, margin.exchanger, ""))

          if(feeInTradingBaseCoin>0) {
            val fee =
              if(margin.exchanger == Poloniex)
                Fee(margin.date, margin.id, feeInTradingBaseCoin, tradingBaseMarket, margin.exchanger, "", alt = Some(feeInTradingBaseCoin*boughtSoldExchangeRate, boughtMarket))
              else
                Fee(margin.date, margin.id, margin.fee, margin.feeMarket, margin.exchanger, "")
            processed ::= preprocessFee(fee)
          }

          processed ::= gainLoss

          if(nonZero(noBasis)) {
            // we are also opening a long
            val longedSoldAmount = soldAmount - closedSoldAmount
            val longedBoughtAmount = boughtAmount - closedBoughtAmount // same as noBasis
            processedAux = openLong(longedSoldAmount, longedBoughtAmount, 0, margin.feeMarket, 0)
          }
        }
      }

      var result = List(Processed.Composed(operationNumber, processed.reverse))
      if(processedAux.nonEmpty)
        result = result ++ List(Processed.Composed(operationNumber, processedAux.reverse))
      return result
    }


    def dispatch(operation: Operation): Unit = {
      operationTracker.setDate(operationNumber, operation.date)
      operationTracker.setExchanger(operationNumber, operation.exchanger)

      operation match {
        case exchange: Exchange =>
          operationTracker.setDescription(operationNumber, "Exchange of %s for %s".format(Format.asMarket(exchange.fromAmount, exchange.fromMarket), Format.asMarket(exchange.toAmount, exchange.toMarket)))
          processedOperations += preprocessExchange(exchange)

        case gain : Gain =>
          operationTracker.setDescription(operationNumber, "Gain of %s".format(Format.asMarket(gain.amount, gain.market)))
          processedOperations += preprocessGain(gain)

        case loss : Loss =>
          operationTracker.setDescription(operationNumber, "Loss of %s".format(Format.asMarket(loss.amount, loss.market)))
          processedOperations += preprocessLoss(loss)

        case fee : Fee =>
          operationTracker.setDescription(operationNumber, "Fee of %s".format(Format.asMarket(fee.amount, fee.market)))
          processedOperations += preprocessFee(fee)

        case margin : Margin =>
          val format =
            if(margin.orderType == Operation.OrderType.Buy)
              "Margin buy of %s with %s"
            else
              "Margin sell of %s for %s"
          operationTracker.setDescription(operationNumber, format.format(Format.asMarket(margin.fromAmount, margin.fromMarket), Format.asMarket(margin.toAmount, margin.toMarket)))
          for(processed <- preprocessMargin(margin))
            processedOperations += processed
      }
    }


    val startDate = Date(2000,1,1)
    val endDate = Date(2500,12,31)
    var currentYear : Int = operations.head.date.getYear
    initializeYear(currentYear)

    var minBTC = Double.MaxValue
    var dateMinBTC = Date(0,0,0)

    for (operation <- operations if operation.date >= startDate && operation.date <= endDate) {
      val newYear = operation.date.getYear
      if(newYear > currentYear) {
        reportYear(currentYear)
        currentYear = initializeYear(newYear)
      }

      operationNumber += 1
      Logger.trace(operationNumber+" "+operation)

      dispatch(operation)

      val m = stocks.apply(Market.bitcoin).totalAmount
      if(m < minBTC) {
        minBTC = m
        dateMinBTC = operation.date
      }

    }
    reportYear(currentYear)

    Logger.trace("Output generated in %s folder.".format(Paths.userOutputFolder))

    println(minBTC, dateMinBTC)
  }
}