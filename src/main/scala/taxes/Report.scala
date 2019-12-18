package taxes

import taxes.date._
import taxes.exchanger._
import taxes.io.FileSystem
import taxes.util.Logger

import scala.collection.mutable.ListBuffer

import spray.json._
import spray.json.JsonProtocol._


object Report {
  // All quantities expressed in base coin
  trait Realized {
    // record realized gains/losses/paid fees per market
    val perMarketGains: ValueTracker
    val perMarketLooses: ValueTracker
    val perMarketPaidFees: ValueTracker // fees are not already deducted from gains and added to looses

    // Difference between these minus fees is realized gains
    val costBasis: ValueTracker  // cost bases of sold coins
    val proceeds: ValueTracker // value obtained for sold coins
  }

  trait State {
    val allStocks: StockPool
    // ledgers and margin stocks for all exchanges are also part of state

    def saveToDisk(year: Int): Unit = {
      val path = FileSystem.stocksFolder(year)
      allStocks.saveToDisk(path)

      for(exchanger <- Exchanger.allExchangers)
        exchanger.saveToDisk(year)
    }

    def loadFromDisk(year: Int): Unit = {
      allStocks.loadFromDisk(FileSystem.stocksFolder(year))
      for(stock <- allStocks)
        stock.ledger.prune()

      for(exchanger <- Exchanger.allExchangers)
        exchanger.loadFromDisk(year)
    }

    def filterBeginningYear(year: Int): Unit = {
      Filters.applyFilters(year, allStocks)
    }
  }

  def process(): Unit = {
    val config = Config.config

    val processedOperations = ListBuffer[Processed]()

    // what market is our basis
    val baseCoin = config.baseCoin
    val baseMarket = baseCoin.market


    object state extends State {
      // current stock of assets with their cost bases (expressed in base coin)
      private val accountingMethod = Config.config.accountingMethod

      val allStocks: StockPool =
        if(accountingMethod == Accounting.FIFO)
          QueueStockPool()
        else if(accountingMethod == Accounting.LIFO)
          StackStockPool()
        else
          Logger.fatal(s"Unkown accounting method: ${Accounting.toString(accountingMethod)}")
    }

    def relevant(operation: Operation): Boolean = config.filterYear match {
      case None =>
        true
      case Some(year) =>
        operation.date.getYear == year
    }

    val operations = Exchanger.preprocessAndReadAllSources().filter(relevant).sortBy(_.date)

    val thisYearOperations = ListBuffer[Operation]()

    if(operations.isEmpty)
      Logger.fatal(s"No operation was found in any exchange for user: ${config.user}.")

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

    def initializeYear(year: Int): Unit = {
      Realized.perMarketGains.clear()
      Realized.perMarketLooses.clear()
      Realized.perMarketPaidFees.clear()

      Realized.costBasis.clear()
      Realized.proceeds.clear()

      partiallyFrees.clear()
      frees.clear()

      operationNumber = 0

      thisYearOperations.clear()
      processedOperations.clear()

      operationTracker.clear()

      state.loadFromDisk(year-1)
      state.filterBeginningYear(year)

      reportPortfolio(year, beginOfYear = true)
    }

    def reportPortfolio(year: Int, beginOfYear: Boolean = false): Unit = {
      val when = if(beginOfYear)"Beginning" else "End"

      val csvFileName = FileSystem.userOutputFolder(year)+s"/Portfolio.$when.$year.csv"
      state.allStocks.printToCSVFile(csvFileName, year)

      val htmlPortfolioFile = s"${FileSystem.userOutputFolder(year)}/Portfolio.$when.$year.html"
      val htmlPortfolioTitle = s"$year $when of year portfolio"
      val htmlPortfolio = HTMLDoc(htmlPortfolioFile, htmlPortfolioTitle)

      htmlPortfolio += state.allStocks.toHTML(htmlPortfolioTitle)

      for(exchanger <- Exchanger.allExchangers.sortBy(_.id))
        exchanger.ledgerPool.summaryToHTML(beginOfYear) match {
          case None => ;
          case Some(html) => htmlPortfolio += html
        }

      htmlPortfolio.close()
    }

    def reportYear(year: Int): Unit = {
      val method = Accounting.toString(Config.config.accountingMethod)

      val htmlReportFile = s"${FileSystem.userOutputFolder(year)}/$method$year.html"
      val htmlReportTitle = s"$year $method Report"
      val htmlReport = HTMLDoc(htmlReportFile, htmlReportTitle)

      htmlReport += <div class='header'>{htmlReportTitle}</div>

      for(processed <- processedOperations)
        htmlReport += processed

      htmlReport += HTMLDoc.reportResults(year, Realized)
      htmlReport.close()

      reportPortfolio(year)

      val htmlExtraFile = s"${FileSystem.userOutputFolder(year)}/Extra$year.html"
      val htmlExtraTitle = s"$year Statistics"
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
          {for(stockPool <- Exchanger.allExchangers.map(_.marginLongs))
          for(stock <- stockPool)
            if(stock.totalAmount>0)
              htmlExtra += <div>{stock.toHTML(showTotal = true)}</div>
          }
        </div>

        {htmlExtra += <div>Opened margin shorts:</div>}
        <div>
          {for(stockPool <- Exchanger.allExchangers.map(_.marginShorts))
          for(stock <- stockPool)
            if(stock.totalAmount>0)
              htmlExtra += <div>{stock.toHTML(showTotal = true)}</div>
          }
        </div>
      }
      htmlExtra.close()


      operationTracker.printToCSVFile("FIFO", year)

      val htmlLedgersFile = s"${FileSystem.userOutputFolder(year)}/Ledgers$year.html"
      val htmlLedgersTitle = s"Ledgers $year"
      val htmlLedgers = HTMLDoc(htmlLedgersFile, htmlLedgersTitle)

      htmlLedgers += <div class='header'>{htmlLedgersTitle}</div>

      htmlLedgers += <div class='header'>Spot markets</div>
      for(stock <- state.allStocks.toList.sortBy(_.market))
        stock.ledger.toHTML(year) match {
          case None => ;
          case Some(html) => htmlLedgers += html
        }

      for(exchanger <- Exchanger.allExchangers)
        for((stockPool, title) <- List((exchanger.marginLongs, "Margin Longs"), (exchanger.marginShorts, "Margin Shorts"))) {
          val htmls = stockPool.toList.sortBy(_.id).flatMap(_.ledger.toHTML(year))

          if(htmls.nonEmpty) {
            htmlLedgers += <div class='header'>{exchanger + "  " + title}</div>
            for(html <- htmls)
              htmlLedgers += html
          }
        }
      htmlLedgers.close()


      for(exchanger <- Exchanger.allExchangers) {
        val htmlExchangerLedgersFile = s"${FileSystem.userOutputFolder(year)}/$exchanger.Ledgers$year.html"
        val htmlExchangerLedgersTitle = s"$exchanger Ledgers $year"
        val htmlExchangerLedgers = HTMLDoc(htmlExchangerLedgersFile, htmlExchangerLedgersTitle)

        exchanger.ledgerPool.toHTML(year) match {
          case None => ;
          case Some(html) =>
            htmlExchangerLedgers += <div class='header'>{htmlExchangerLedgersTitle}</div>
            htmlExchangerLedgers += html
        }

        for((marginStockPool, title) <- List((exchanger.marginLongs, "Margin Longs"), (exchanger.marginShorts, "Margin Shorts"))) {
          val htmls = marginStockPool.toList.sortBy(_.id).flatMap(_.ledger.toHTML(year))

          if(htmls.nonEmpty) {
            htmlExchangerLedgers += <div class='header'>{title}</div>
            for(html <- htmls)
              htmlExchangerLedgers += html
          }
        }
        htmlExchangerLedgers.close()
      }
    }

    def finalizeYear(year: Int): Unit = {
      reportYear(year)

      // save state
      state.saveToDisk(year)

      // save some more information
      Market.saveToFile(FileSystem.marketFile(year))
      Config.config.saveToFile(FileSystem.configFile(year))
      FileSystem.withPrintStream(FileSystem.operationsFile(year)){ ps =>
        ps.print(thisYearOperations.toList.toJson.prettyPrint)
      }

      FileSystem.withPrintStream(FileSystem.processedOperationsFile(year)){ ps =>
        ps.print(processedOperations.toList.toJson.prettyPrint)
      }
    }

    def simplify(processed: Seq[Processed]): Processed =
      if(processed.length>1)
        Processed.Composed(operationNumber, processed)
      else
        processed.head


    def marginPairKey(market1: Market, market2: Market): String
      = market1 ++ "-" ++ market2


    def preprocessExchange(exchange: Exchange): Processed =
      if(Config.config.deprecatedUp2017Version)
        _deprecated_preprocessExchange(exchange)
      else
        _preprocessExchange(exchange)


    def _preprocessExchange(exchange: Exchange): Processed = {
      val soldMarket = exchange.fromMarket
      val soldAmount = exchange.fromAmount // without fee

      val boughtMarket = exchange.toMarket
      val boughtAmount = exchange.toAmount // without fee

      val fees = exchange.fees

      // Compute totals on both sides of exchange including fees
      var totalSoldAmount = soldAmount
      var totalBoughtAmount = boughtAmount
      for(fee <- fees) {
        if(fee.amount == 0) {
          ;
        } else if(fee.market == soldMarket)
          totalSoldAmount += fee.amount
        else if(fee.market == boughtMarket)
          totalBoughtAmount += fee.amount
      }

      if(totalSoldAmount==0)
        Logger.warning(s"No sold coins in this exchange $exchange.")
      if(totalBoughtAmount==0)
        Logger.warning(s"No bought coins in this exchange $exchange.")

      // Fees expressed in `boughtMarket' are considered part of the exchange.
      // Those expressed in `soldMarket' are not.

      // Compute exchange rates (fee in buy side is included)
      val (boughtSoldExchangeRate, soldBoughtExchangeRate) =
        (totalBoughtAmount / soldAmount, soldAmount / totalBoughtAmount)

      // Total value involved in this exchange, expressed in base coin
      // and proxy used to compute (base coin expressed) prices
      val (totalInBaseCoin, baseCoinProxy, baseCoinProxyRate) =
        if(soldMarket == baseMarket)
          (soldAmount, baseMarket, 1.0)  // fees expressed in `soldMarket' are not included
        else if(boughtMarket == baseMarket)
          (totalBoughtAmount, baseMarket, 1.0) // fees expressed in `bougthMarket' are included
        else if(Market.priority(soldMarket) > Market.priority(boughtMarket)) {
          val rate = baseCoin.priceInBaseCoin(soldMarket, exchange.date)
          (soldAmount * rate, soldMarket, rate) // fees expressed in `soldMarket' are not included
        } else {
          val rate = baseCoin.priceInBaseCoin(boughtMarket, exchange.date)
          (totalBoughtAmount * rate, boughtMarket, rate) // fees expressed in `boughtMarket' are included
        }

      // Price we sold released coins at, expressed in base coin
      // fees expressed in `soldMarket' are not included
      val soldPriceInBaseCoin = totalInBaseCoin / soldAmount

      // Price we bought acquired coins at, expressed in base coin
      // fees expressed in `boughtMarket' are included
      val boughtBasisPriceInBaseCoin = totalInBaseCoin / totalBoughtAmount

      // Add all bought coins with their cost basis to our stock of assets.
      // Note that fee in `boughtMarket' are included as they will be deducted right after as a fee
      if(!boughtBasisPriceInBaseCoin.isNaN && totalBoughtAmount > 0) {
        state.allStocks.add(boughtMarket, totalBoughtAmount, boughtBasisPriceInBaseCoin, exchange.exchanger, exchange.date, soldBoughtExchangeRate, soldMarket, s"$operationNumber Exchange")(baseMarket)
        exchange.exchanger.ledgerPool.record(boughtMarket)(exchange.date, totalBoughtAmount, exchange.exchanger, s"$operationNumber Exchange")
      }

      // Get cost basis for sold coins from current stock
      // fees expressed in `soldMarket' are not included
      val (soldBasisInBaseCoin, noBasis, usedStocks) = {
        exchange.exchanger.ledgerPool.record(soldMarket)(exchange.date, -soldAmount, exchange.exchanger, s"$operationNumber Exchange")

        val t3 = state.allStocks.remove(soldMarket, soldAmount)(baseMarket)(exchange.date, exchange.exchanger, s"$operationNumber Exchange")
        if(soldMarket != baseMarket)
          t3
        else
        // If our base coin is Euro, cost basis is the amount of Euros sold.
        // Try to remove those coins from our stock of Euros, but basis is
        // the amount sold, even if they are not in our stock
          (soldAmount, 0.0, t3._3)
      }

      // Value of sold coins, expressed in base coin
      // fees expressed in `soldMarket' are not included
      val proceedsInBaseCoin =
        if(soldAmount==0)
          0
        else
          totalInBaseCoin


      // Gain in this exchange, expressed in base coin.
      // Negative in case it's a loss
      val gainInBaseCoin = proceedsInBaseCoin - soldBasisInBaseCoin


      // Update total gains/looses for soldMarket
      if(gainInBaseCoin > 0)
        Realized.perMarketGains.record(soldMarket, gainInBaseCoin)
      else if(gainInBaseCoin < 0)
        Realized.perMarketLooses.record(soldMarket, gainInBaseCoin.abs)


      // If your base market is Euros, selling Euros is neither a gain nor a loss.
      // In this case proceedsInBaseCoin will be equal to soldBasisInBaseCoin.
      // We can add soldBasisInBaseCoin to our list of costBasis and
      // proceedsInBaseCoin to our list of proceeds or we can omit both of them.
      // In both cases, net gains will be the same as local result of this exchange
      // will be 0. If `addBaseMarketSells' is true we take the first approach and add both.
      val addBaseMarketSells = true

      // Update realized cost basis and proceeds
      if(soldMarket != baseMarket || addBaseMarketSells) {
        Realized.costBasis.record(soldMarket, soldBasisInBaseCoin)
        Realized.proceeds.record(soldMarket, proceedsInBaseCoin)

        operationTracker.recordCostBasis(operationNumber, soldBasisInBaseCoin)
        operationTracker.recordProceeds(operationNumber, proceedsInBaseCoin)
      }


      if(soldMarket != baseMarket && soldBasisInBaseCoin == 0)
        frees += exchange // These assets were acquired for free

      if(soldMarket != baseMarket && noBasis.abs > 0.01)
      // Part of these assets were acquired for free
        partiallyFrees += f"Was SOLD but were partially free $noBasis%.8f of $totalSoldAmount%.6f $soldMarket = ${noBasis * proceedsInBaseCoin / soldAmount}%.8f $baseMarket"


      // processed operations resulting from this exchange
      var processed = List[Processed]()

      // process all fees involved in this operation. Note that processing them
      // removes corresponding amounts from our stock of coins and records the
      // fee as part of the current operation.
      for(fee <- fees; if fee.amount>0)
        processed :+=
          preprocessFee(Fee(
            date = exchange.date
            , id = exchange.id
            , amount = fee.amount
            , market = fee.market
            , exchanger = exchange.exchanger
            , description = ""
            , alt = fee.alt
          ))


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
          , _deprecated_feeAmount = 0 // Not useful anymore for _preprocessExchange
          , _deprecated_feeMarket = "" // Not useful anymore for _preprocessExchange
          , _deprecated_feeInBaseCoin = 0 // feeInBaseCoin. Not useful anymore for _preprocessExchange
          , gainInBaseCoin = gainInBaseCoin
          , boughtSoldExchangeRate = boughtSoldExchangeRate
          , soldBoughtExchangeRate = soldBoughtExchangeRate
          , usedStocks = usedStocks
          , buys = state.allStocks(boughtMarket)(baseMarket).copy
          , sells = state.allStocks(soldMarket)(baseMarket).copy
        )


      // A settlement is bought to pay some dues so this is like a buy followed
      // by a loss but
      if(exchange.isSettlement && exchange.exchanger == Poloniex) {
        val marketKey = marginPairKey(exchange.toMarket, exchange.fromMarket)

        val stockContainer = Poloniex.marginShorts(marketKey)(baseMarket)
        if(stockContainer.nonEmpty) {
          stockContainer.removeAndGetBasis(totalBoughtAmount)(exchange.date, exchange.exchanger, exchange.description)
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

      processed ::= processedExchange // order for a Composed is important. Exchange must be at front
      return simplify(processed)
    }


    def preprocessFee(fee: Fee): Processed.Fee = {
      fee.exchanger.ledgerPool.record(fee.market)(fee.date, -fee.amount, fee.exchanger, s"$operationNumber Fee")

      val (feeInBaseCoin0, _, usedStocks) = state.allStocks.remove(fee.market, fee.amount)(baseMarket)(fee.date, fee.exchanger, s"$operationNumber Fee")

      val feeInBaseCoin = if(fee.market == baseMarket) fee.amount else feeInBaseCoin0

      // Record paid fees
      Realized.perMarketPaidFees.record(fee.market, feeInBaseCoin)

      operationTracker.recordFee(operationNumber, feeInBaseCoin)

      return Processed.Fee(
        operationNumber = operationNumber
        , fee = fee
        , feeInBaseCoin = feeInBaseCoin
        , usedStocks = usedStocks
        , stocks = state.allStocks(fee.market)(baseMarket).copy
      )
    }


    def preprocessLoss(loss: Loss): Processed.Loss = {
      loss.exchanger.ledgerPool.record(loss.market)(loss.date, -loss.amount, loss.exchanger, s"$operationNumber Loss")

      val (lossBasisInBaseCoin0, _, usedStocks) = state.allStocks.remove(loss.market, loss.amount)(baseMarket)(loss.date, loss.exchanger, s"$operationNumber Loss")

      val lossBasisInBaseCoin = if(loss.market == baseMarket) loss.amount else lossBasisInBaseCoin0

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
        , stocks = state.allStocks(loss.market)(baseMarket).copy
      )
    }


    def preprocessGain(gain: Gain): Processed.Gain = {
      // Record cost basis of gained coins at price corresponding to gain date
      val basePrice = baseCoin.priceInBaseCoin(gain.market, gain.date)
      state.allStocks.add(gain.market, gain.amount, basePrice, gain.exchanger, gain.date, basePrice, baseMarket, s"$operationNumber Gain")(baseMarket)
      gain.exchanger.ledgerPool.record(gain.market)(gain.date, gain.amount, gain.exchanger, s"$operationNumber Gain")

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
        , stocks = state.allStocks(gain.market)(baseMarket).copy
      )
    }


    def preprocessMargin(margin: Margin): Seq[Processed] = {
      val soldMarket = margin.fromMarket
      val soldAmount = margin.fromAmount

      val boughtMarket = margin.toMarket
      val boughtAmount = margin.toAmount

      val fees = margin.fees

      // gain/looses are expressed in marginBaseMarket
      val (tradedMarket, marginBaseMarket) = margin.pair

      val marketKey = marginPairKey(tradedMarket, marginBaseMarket)

      // Check that markets involved in margin operation are those in traded pair
      if(margin.orderType == Operation.OrderType.Sell) {
        if(soldMarket != tradedMarket || boughtMarket != marginBaseMarket)
          Logger.fatal(s"Wrong markets in margin sell: $margin")
      } else if(margin.orderType == Operation.OrderType.Buy) {
        if(soldMarket != marginBaseMarket || boughtMarket != tradedMarket)
          Logger.fatal(s"Wrong markets in margin buy: $margin")
      }

      // Compute margin exchange rates
      val (boughtSoldExchangeRate, soldBoughtExchangeRate) =
        (boughtAmount / soldAmount, soldAmount / boughtAmount)

      def processFees(fees: Seq[FeePair], poloniexConversion: Boolean): List[Processed] = {
        // The fee for this operation, expressed in margin base coin. This is only for Poloniex
        var feeInMarginBaseCoin = 0.0
        if(poloniexConversion)
          for(fee <- fees) {
            if(fee.amount == 0) {
              ;
            } else if(fee.market == marginBaseMarket)
              feeInMarginBaseCoin += fee.amount
            else if(fee.market == boughtMarket && soldMarket == marginBaseMarket)
              feeInMarginBaseCoin += soldBoughtExchangeRate * fee.amount
            else if(fee.market == soldMarket && boughtMarket == marginBaseMarket)
              feeInMarginBaseCoin += boughtSoldExchangeRate * fee.amount
            else {
              ;
            }
          }

        var processedFees = List[Processed]()
        if(poloniexConversion && margin.exchanger == Poloniex && feeInMarginBaseCoin > 0) {
          processedFees ::= preprocessFee(
            Fee(margin.date
            , margin.id
            , feeInMarginBaseCoin
            , marginBaseMarket
            , margin.exchanger
            , ""
            , Some(feeInMarginBaseCoin * boughtSoldExchangeRate, boughtMarket)
            )
          )
        } else {
          for(fee <- fees; if fee.amount > 0)
            processedFees :+= preprocessFee(
              Fee(margin.date
              , margin.id
              , fee.amount
              , fee.market
              , margin.exchanger
              , ""
              , alt = fee.alt
              )
            )
        }
        processedFees
      }

      val marginLongs = margin.exchanger.marginLongs
      val marginShorts = margin.exchanger.marginShorts

      var processed = List[Processed]()
      var processedExtra = List[Processed]()

      def nonZero(x: Double): Boolean = x.abs >= 1E-7

      def openShort(soldAmount: Double, boughtAmount: Double, fees: Seq[FeePair]): List[Processed] = {
        // Opening a short
        marginShorts.add(marketKey, soldMarket, soldAmount, boughtSoldExchangeRate, margin.exchanger, margin.date, boughtSoldExchangeRate, boughtMarket, s"$operationNumber Open Short")(marginBaseMarket)

        var processed = List[Processed]()

        processed ::=
          Processed.Margin(
            operationNumber = operationNumber
            , date = margin.date
            , exchanger = margin.exchanger
            , what = "Open short"
            , fromAmount = soldAmount, fromMarket = soldMarket
            , toAmount = boughtAmount, toMarket = boughtMarket
            , exchangeRate = boughtSoldExchangeRate
            , description = margin.description
            , usedStocksOpt = None
            , marginLongs = marginLongs(marketKey)(marginBaseMarket).copy
            , marginShorts = marginShorts(marketKey)(marginBaseMarket).copy
          )

        processed :::= processFees(fees, poloniexConversion = false)

        return processed
      }

      def openLong(soldAmount: Double, boughtAmount: Double, fees: Seq[FeePair]): List[Processed] = {
        // Opening a long
        marginLongs.add(marketKey, boughtMarket, boughtAmount, soldBoughtExchangeRate, margin.exchanger, margin.date, soldBoughtExchangeRate, soldMarket, s"$operationNumber Open Long")(marginBaseMarket)

        var processed = List[Processed]()

        processed ::=
          Processed.Margin(
            operationNumber = operationNumber
            , date = margin.date
            , exchanger = margin.exchanger
            , what = "Open long"
            , fromAmount = soldAmount, fromMarket = soldMarket
            , toAmount = boughtAmount, toMarket = boughtMarket
            , exchangeRate = soldBoughtExchangeRate
            , description = margin.description
            , usedStocksOpt = None
            , marginLongs = marginLongs(marketKey)(marginBaseMarket).copy
            , marginShorts = marginShorts(marketKey)(marginBaseMarket).copy
          )

        processed :::= processFees(fees, poloniexConversion = true)

        return processed
      }

      if(margin.orderType == Operation.OrderType.Sell) {
        val inLongsAmount = marginLongs(marketKey)(marginBaseMarket).totalAmount

        if(inLongsAmount<=0) {
          // Opening a short
          processed :::= openShort(soldAmount, boughtAmount, fees)
        } else {
          // Closing a long
          val closedSoldAmount = soldAmount min inLongsAmount
          val closedBoughtAmount = closedSoldAmount * boughtSoldExchangeRate

          val (basis, noBasis, usedStocks) = marginLongs.remove(marketKey, soldAmount)(baseMarket)(margin.date, margin.exchanger, s"$operationNumber Close Long")

          processed ::=
            Processed.Margin(
              operationNumber = operationNumber
              , date = margin.date
              , exchanger = margin.exchanger
              , what = "Close long"
              , fromAmount = closedSoldAmount, fromMarket = soldMarket
              , toAmount = closedBoughtAmount, toMarket = boughtMarket
              , exchangeRate = boughtSoldExchangeRate
              , description = margin.description
              , usedStocksOpt = Some(usedStocks)
              , marginLongs = marginLongs(marketKey)(marginBaseMarket).copy
              , marginShorts = marginShorts(marketKey)(marginBaseMarket).copy
            )

          val gain = closedBoughtAmount - basis

          val gainLoss =
            if(gain>0)
              preprocessGain(Gain(margin.date, margin.id, gain, marginBaseMarket, margin.exchanger, ""))
            else
              preprocessLoss(Loss(margin.date, margin.id, gain.abs, marginBaseMarket, margin.exchanger, ""))

          processed :::= processFees(fees, poloniexConversion = false)

          processed ::= gainLoss

          if(nonZero(noBasis)) {
            // we are also opening a short
            val longedSoldAmount = soldAmount - closedSoldAmount // same as noBasis
            val longedBoughtAmount = boughtAmount - closedBoughtAmount
            processedExtra = openShort(longedSoldAmount, longedBoughtAmount, List())
          }
        }
      } else if(margin.orderType == Operation.OrderType.Buy) {
        val inShortsAmount = marginShorts(marketKey)(marginBaseMarket).totalAmount

        if(inShortsAmount<=0) {
          // Opening a long
          processed :::= openLong(soldAmount, boughtAmount, fees)
        } else {
          // Closing a short
          val closedBoughtAmount = boughtAmount min inShortsAmount
          val closedSoldAmount = closedBoughtAmount * soldBoughtExchangeRate

          val (basis, noBasis, usedStocks) = marginShorts.remove(marketKey, boughtAmount)(baseMarket)(margin.date, margin.exchanger, s"$operationNumber Close Short")

          processed ::=
            Processed.Margin(
              operationNumber = operationNumber
              , date = margin.date
              , exchanger = margin.exchanger
              , what = "Close short"
              , fromAmount = closedSoldAmount, fromMarket = soldMarket
              , toAmount = closedBoughtAmount, toMarket = boughtMarket
              , exchangeRate = soldBoughtExchangeRate
              , description = margin.description
              , usedStocksOpt = Some(usedStocks)
              , marginLongs = marginLongs(marketKey)(marginBaseMarket).copy
              , marginShorts = marginShorts(marketKey)(marginBaseMarket).copy
            )

          val gain = basis - closedSoldAmount

          val gainLoss =
            if(gain>0)
              preprocessGain(Gain(margin.date, margin.id, gain, marginBaseMarket, margin.exchanger, "") /*, -closedSoldAmount*/)
            else
              preprocessLoss(Loss(margin.date, margin.id, gain.abs, marginBaseMarket, margin.exchanger, ""))

          processed :::= processFees(fees, poloniexConversion = true)

          processed ::= gainLoss

          if(nonZero(noBasis)) {
            // we are also opening a long
            val longedSoldAmount = soldAmount - closedSoldAmount
            val longedBoughtAmount = boughtAmount - closedBoughtAmount // same as noBasis
            processedExtra = openLong(longedSoldAmount, longedBoughtAmount, List())
          }
        }
      }

      var result = List(Processed.Composed(operationNumber, processed.reverse))
      if(processedExtra.nonEmpty)
        result ++= List(Processed.Composed(operationNumber, processedExtra.reverse))
      return result
    }

    def trackable(operation: Operation): Boolean = operation match {
      case d: Deposit         => false
      case w: Withdrawal      => false
      case ntf: NonTaxableFee => false
      case _                  => true
    }

    def dispatch(operation: Operation): Unit = {
      if(trackable(operation)) {
        operationTracker.setDate(operationNumber, operation.date)
        operationTracker.setExchanger(operationNumber, operation.exchanger)
      }

      operation match {
        case exchange: Exchange =>
          operationTracker.setDescription(operationNumber, s"Exchange of ${Format.asMarket(exchange.fromAmount, exchange.fromMarket)} for ${Format.asMarket(exchange.toAmount, exchange.toMarket)}")
          processedOperations += preprocessExchange(exchange)

        case gain: Gain =>
          operationTracker.setDescription(operationNumber, s"Gain of ${Format.asMarket(gain.amount, gain.market)}")
          processedOperations += preprocessGain(gain)

        case loss: Loss =>
          operationTracker.setDescription(operationNumber, s"Loss of ${Format.asMarket(loss.amount, loss.market)}")
          processedOperations += preprocessLoss(loss)

        case fee: Fee =>
          operationTracker.setDescription(operationNumber, s"Fee of ${Format.asMarket(fee.amount, fee.market)}")
          processedOperations += preprocessFee(fee)

        case margin: Margin =>
          val format =
            if(margin.orderType == Operation.OrderType.Buy)
              "Margin buy of %s with %s"
            else
              "Margin sell of %s for %s"
          operationTracker.setDescription(operationNumber, format.format(Format.asMarket(margin.fromAmount, margin.fromMarket), Format.asMarket(margin.toAmount, margin.toMarket)))
          for(processed <- preprocessMargin(margin))
            processedOperations += processed

        case deposit: Deposit =>
          deposit.exchanger.ledgerPool.record(deposit.market)(deposit.date, deposit.amount, deposit.exchanger, deposit.description)

        case withdrawal: Withdrawal =>
          withdrawal.exchanger.ledgerPool.record(withdrawal.market)(withdrawal.date, -withdrawal.amount, withdrawal.exchanger, withdrawal.description)

        case nonTaxableFee: NonTaxableFee =>
          nonTaxableFee.exchanger.ledgerPool.record(nonTaxableFee.market)(nonTaxableFee.date, -nonTaxableFee.amount, nonTaxableFee.exchanger, nonTaxableFee.description)
          // should be here but breaks back compatibility
          // state.stocks.remove(nonTaxableFee.market, nonTaxableFee.amount)(baseMarket)(nonTaxableFee.date, nonTaxableFee.exchanger, nonTaxableFee.description)


      }
    }

    def _deprecated_preprocessExchange(exchange: Exchange): Processed = {
      val soldMarket = exchange.fromMarket
      val soldAmount = exchange.fromAmount // without fee

      val boughtMarket = exchange.toMarket
      val boughtAmount = exchange.toAmount // without fee

      // extract fee and detached fee from fees list
      val (xs, ys) =
        exchange.fees.filter(fee => fee.amount>0)
        .partition(fee => Set(soldMarket, boughtMarket).contains(fee.market))

      val (feeAmount, feeMarket, zs) = xs match {
        case Seq() =>
          (0.0, boughtMarket, ys)
        case Seq(fee, fees @ _*) =>
          (fee.amount, fee.market, fees ++ ys)
      }

      val detachedFeeOpt = zs match {
        case Seq() =>
          None
        case Seq(fee, fees @ _*) =>
          Some(FeePair(fee.amount, fee.market))
      }

      val (totalSoldAmount, totalBoughtAmount) = // including fee
        if(feeAmount == 0)
          (soldAmount, boughtAmount)
        else if(feeMarket == soldMarket)
          (soldAmount + feeAmount, boughtAmount)
        else if(feeMarket == boughtMarket)
          (soldAmount, boughtAmount + feeAmount)
        else
          Logger.fatal(s"Cannot process exchange as fee is not expressed in same unit as from or to markets.${exchange.toString}")


      if(totalSoldAmount==0)
        Logger.warning(s"No sold coins in this exchange $exchange.")
      if(totalBoughtAmount==0)
        Logger.warning(s"No bought coins in this exchange $exchange.")


      // Compute exchange rates (fee is included)
      val (boughtSoldExchangeRate, soldBoughtExchangeRate) =
        (totalBoughtAmount / totalSoldAmount, totalSoldAmount / totalBoughtAmount)

      // Total value involved in this operation, expressed in base coin
      // and proxy used to compute (base coin expressed) prices
      val (totalInBaseCoin, baseCoinProxy, baseCoinProxyRate) =
      if(soldMarket == baseMarket)
        (totalSoldAmount, baseMarket, 1.0)
      else if(boughtMarket == baseMarket)
        (totalBoughtAmount, baseMarket, 1.0)
      else if(Market.priority(soldMarket) > Market.priority(boughtMarket)) {
        val rate = baseCoin.priceInBaseCoin(soldMarket, exchange.date)
        (totalSoldAmount * rate, soldMarket, rate)
      } else {
        val rate = baseCoin.priceInBaseCoin(boughtMarket, exchange.date)
        (totalBoughtAmount * rate, boughtMarket, rate)
      }

      // fee for this operation, expressed in base coin,
      // only if fee is included in this exchange
      val feeInBaseCoin =
        if(feeAmount == 0) // we don't care about feemarket in this case
          0
        else if(feeMarket == baseMarket)
          feeAmount
        else if(feeMarket == boughtMarket && soldMarket == baseMarket)
          soldBoughtExchangeRate * feeAmount
        else if(feeMarket == soldMarket && boughtMarket == baseMarket)
          boughtSoldExchangeRate * feeAmount
        else if(feeMarket == boughtMarket)
          feeAmount * totalInBaseCoin / totalBoughtAmount
        else if(feeMarket == soldMarket)
          feeAmount * totalInBaseCoin / totalSoldAmount
        else
          Logger.fatal(s"Cannot process exchange as fee is not expressed in same unit as from or to markets.${exchange.toString}")

      // Price we sold released coins at, expressed in base coin
      val soldPriceInBaseCoin = totalInBaseCoin / totalSoldAmount

      // Price we bought acquired coins at, expressed in base coin
      val boughtBasisPriceInBaseCoin = totalInBaseCoin / totalBoughtAmount

      // Add bought coins (without paid fee) with their cost basis to our stock of assets
      if(!boughtBasisPriceInBaseCoin.isNaN && boughtAmount > 0) {
        state.allStocks.add(boughtMarket, boughtAmount, boughtBasisPriceInBaseCoin, exchange.exchanger, exchange.date, soldBoughtExchangeRate, soldMarket, s"$operationNumber Exchange")(baseMarket)
        exchange.exchanger.ledgerPool.record(boughtMarket)(exchange.date, boughtAmount, exchange.exchanger, s"$operationNumber Exchange")
      }

      // Get cost basis for total sold coins from current stock
      val (totalSoldBasisInBaseCoin, noBasis, usedStocks) =
        if(soldMarket != baseMarket) {
          exchange.exchanger.ledgerPool.record(soldMarket)(exchange.date, -totalSoldAmount, exchange.exchanger, s"$operationNumber Exchange")
          state.allStocks.remove(soldMarket, totalSoldAmount)(baseMarket)(exchange.date, exchange.exchanger, s"$operationNumber Exchange")
        } else {
          // If our base coin is Euro, cost basis is the amount of Euros involved
          exchange.exchanger.ledgerPool.record(soldMarket)(exchange.date, -totalSoldAmount, exchange.exchanger, s"$operationNumber Exchange")
          val (_,_,usedStocks) = state.allStocks.remove(soldMarket, totalSoldAmount)(baseMarket)(exchange.date, exchange.exchanger, s"$operationNumber Exchange")
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
        if(soldMarket == baseMarket) // If base coin is Euros, releasing Euros makes no profit
          0
        else
          proceedsInBaseCoin - soldBasisInBaseCoin

      // Update paid fees
      Realized.perMarketPaidFees.record(feeMarket, feeInBaseCoin)

      // Update total gains for soldMarket
      if(gainInBaseCoin > 0)
        Realized.perMarketGains.record(soldMarket, gainInBaseCoin)
      else if(gainInBaseCoin < 0)
        Realized.perMarketLooses.record(soldMarket, gainInBaseCoin.abs)

      // Update realized cost basis and proceeds
      if(soldMarket == baseMarket) {
        // this is the case for Euro -> Any Thing if your base is Euro.
        ;
      } else {
        Realized.costBasis.record(soldMarket, soldBasisInBaseCoin)
        Realized.proceeds.record(soldMarket, proceedsInBaseCoin)
      }

      if(totalSoldBasisInBaseCoin == 0 && (soldMarket != baseMarket))
        frees += exchange // These assets were acquired for free

      if(noBasis.abs > 0.01 && (soldMarket != baseMarket))
      // Part of these assets were acquired for free
        partiallyFrees += f"Was SOLD but were partially free $noBasis%.8f of $totalSoldAmount%.6f $soldMarket = ${noBasis * proceedsInBaseCoin / soldAmount}%.8f $baseMarket"

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
          , _deprecated_feeAmount = feeAmount
          , _deprecated_feeMarket = feeMarket
          , _deprecated_feeInBaseCoin = feeInBaseCoin
          , gainInBaseCoin = gainInBaseCoin
          , boughtSoldExchangeRate = boughtSoldExchangeRate
          , soldBoughtExchangeRate = soldBoughtExchangeRate
          , usedStocks = usedStocks
          , buys = state.allStocks(boughtMarket)(baseMarket).copy
          , sells = state.allStocks(soldMarket)(baseMarket).copy
        )

      var processed = List[Processed]()

      // A settlement is bought to pay some dues so this is like a buy followed
      // by a loss but
      if(exchange.isSettlement && exchange.exchanger == Poloniex) {
        val marketKey = marginPairKey(exchange.toMarket, exchange.fromMarket)

        val stockContainer = Poloniex.marginShorts(marketKey)(baseMarket)
        if(stockContainer.nonEmpty) {
          stockContainer.removeAndGetBasis(totalBoughtAmount)(exchange.date, exchange.exchanger, exchange.description)
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

      // A fee that is not part of the exchange is added to composed result
      detachedFeeOpt match {
        case None => ;
        case Some(fee) =>
          val processedFee = preprocessFee(
            Fee(
              date = exchange.date
              , id = exchange.id
              , amount = fee.amount
              , market = fee.market
              , exchanger = exchange.exchanger
              , description = ""
            ))
          processed ::= processedFee
      }

      processed ::= processedExchange // order for a Composed is important. Exchange must be at front
      return simplify(processed)
    }

    var currentYear: Int = -1

    var minBTC = Double.MaxValue
    var dateMinBTC = LocalDateTime()

    for(operation <- operations) {
      val newYear = operation.date.getYear
      if(newYear > currentYear) {
        if(currentYear > 0)
          finalizeYear(currentYear)

        initializeYear(newYear)
        currentYear = newYear
      }

      if(trackable(operation))
        operationNumber += 1

      val label =
        if(trackable(operation))
          operationNumber.toString
        else
          "".padTo(operationNumber.toString.length, ' ')
      Logger.trace(s"*** $label $operation")

      thisYearOperations += operation

      dispatch(operation)

      val m = state.allStocks(Market.bitcoin)(baseMarket).totalAmount
      if(m < minBTC) {
        minBTC = m
        dateMinBTC = operation.date
      }

    }
    finalizeYear(currentYear)

    Logger.trace(s"Output generated in ${FileSystem.userOutputFolder} folder.")

    println(minBTC, dateMinBTC)
  }
}
