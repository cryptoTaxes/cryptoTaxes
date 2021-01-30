package taxes

import taxes.date._
import taxes.exchanger._
import taxes.io.FileSystem
import taxes.util.Logger

import scala.collection.mutable.ListBuffer

import spray.json._
import spray.json.JsonProtocol._


object Report {
  // All quantities expressed in base currency
  trait Realized {
    // record realized gains/losses/paid fees per currency
    val perCurrencyGains: ValueTracker
    val perCurrencyLooses: ValueTracker
    val perCurrencyPaidFees: ValueTracker // fees are not already deducted from gains and added to looses

    // Difference between these minus fees is realized gains
    val costBasis: ValueTracker  // cost bases of sold currencies
    val proceeds: ValueTracker   // value obtained for sold currencies
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

    // what currency is our basis
    val basis = config.baseCurrency
    val baseCurrency = basis.currency


    object state extends State {
      // current stock of assets with their cost bases (expressed in base currency)
      private val accountingMethod = Config.config.accountingMethod

      val allStocks: StockPool =
        if(accountingMethod == Accounting.FIFO)
          QueueStockPool()
        else if(accountingMethod == Accounting.LIFO)
          StackStockPool()
        else
          Logger.fatal(s"Unknown accounting method: ${Accounting.toString(accountingMethod)}")
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

    // All quantities expressed in base currency
    object Realized extends Realized {
      // record realized gains/losses/paid fees per currency
      val perCurrencyGains = ValueTracker(baseCurrency)
      val perCurrencyLooses = ValueTracker(baseCurrency)
      val perCurrencyPaidFees = ValueTracker(baseCurrency) // fees are not already deducted from gains and added to looses

      // Difference between these minus fees is realized gains
      val costBasis = ValueTracker(baseCurrency)  // cost bases of sold currencies
      val proceeds = ValueTracker(baseCurrency) // value obtained for sold currencies
    }

    // order of current operation within current year
    var operationNumber = 0

    var currentYear: Int = -1
    def linkToCurrentOperation(what: String): RichText =
      RichText(s"${RichText.report(currentYear, operationNumber)} $what")

    val operationTracker = OperationTracker()

    def initializeYear(year: Int): Unit = {
      Realized.perCurrencyGains.clear()
      Realized.perCurrencyLooses.clear()
      Realized.perCurrencyPaidFees.clear()

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
      val when = if(beginOfYear) "Beginning" else "End"
      val title = s"$year $when of year portfolio"

      val csvFileName = FileSystem.userOutputFolder(year)+s"/Portfolio.$when.$year.csv"
      state.allStocks.printToCSVFile(csvFileName, year, title)

      val htmlPortfolioFile = s"${FileSystem.userOutputFolder(year)}/Portfolio.$when.$year.html"
      val htmlPortfolioTitle = title
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

      val htmlReportFile = FileSystem.report(year, "html")
      val htmlReportTitle = s"$year $method Report"
      val htmlReport = HTMLDoc(htmlReportFile, htmlReportTitle)

      htmlReport += <div class='header'>{htmlReportTitle}</div>

      for(processed <- processedOperations)
        htmlReport += processed

      htmlReport += HTMLDoc.reportResults(year, Realized)
      htmlReport.close()


      val csvlReportFile = FileSystem.report(year, "csv")
      operationTracker.printToCSVFile(csvlReportFile, year, baseCurrency)

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




      val htmlLedgersFile = s"${FileSystem.userOutputFolder(year)}/Ledgers$year.html"
      val htmlLedgersTitle = s"Ledgers $year"
      val htmlLedgers = HTMLDoc(htmlLedgersFile, htmlLedgersTitle)

      htmlLedgers += <div class='header'>{htmlLedgersTitle}</div>

      htmlLedgers += <div class='header'>Spot currencies</div>
      for(stock <- state.allStocks.toList.sortBy(_.currency))
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

      val htmlDisposedStocksFile = s"${FileSystem.userOutputFolder(year)}/DisposedStocks$year.html"
      val htmlDisposedStocksTitle = s"Disposed Stocks $year"
      val htmlDisposedStocks = HTMLDoc(htmlDisposedStocksFile, htmlDisposedStocksTitle)

      htmlDisposedStocks += <div class='header'>{htmlDisposedStocksTitle}</div>
      for(stock <- state.allStocks.toList.sortBy(_.currency))
        stock.disposals.toHTML match {
          case None => ;
          case Some(html) => htmlDisposedStocks += html
        }
      htmlDisposedStocks.close()

      val csvDisposedStocksFile = s"${FileSystem.userOutputFolder(year)}/DisposedStocks$year.csv"
      val csvDisposedStocksTitle = s"Disposed Stocks $year"
      FileSystem.withPrintStream(csvDisposedStocksFile) { ps =>
        ps.println(csvDisposedStocksTitle)
        ps.println()
        for(stock <- state.allStocks.toList.sortBy(_.currency))
          stock.disposals.printToCSV(ps)
      }

      for(exchanger <- Exchanger.allExchangers) {
        val htmlExchangerLedgersFile = s"${FileSystem.userOutputFolder(year)}/Exchanger.$exchanger.Ledgers$year.html"
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
      Currency.saveToFile(FileSystem.currencyFile(year))
      Config.config.saveToFile(FileSystem.configFile(year))
      FileSystem.withPrintStream(FileSystem.operationsFile(year)){ ps =>
        spray.json.PrintStream.prettyPrintOn(ps, thisYearOperations)
      }

      FileSystem.withPrintStream(FileSystem.processedOperationsFile(year)) { ps =>
        spray.json.PrintStream.prettyPrintOn(ps, processedOperations)
      }
    }

    def simplify(processed: Seq[Processed]): Processed =
      if(processed.length>1)
        Processed.Composed(operationNumber, processed)
      else
        processed.head


    def marginPairKey(currency1: Currency, currency2: Currency): String
      = currency1 ++ "-" ++ currency2


    def preprocessExchange(exchange: Exchange): Processed =
      if(Config.config.deprecatedUp2017Version)
        _deprecated_preprocessExchange(exchange)
      else
        _preprocessExchange(exchange)


    def _preprocessExchange(exchange: Exchange): Processed = {
      val soldCurrency = exchange.fromCurrency
      val soldAmount = exchange.fromAmount // without fee

      val boughtCurrency = exchange.toCurrency
      val boughtAmount = exchange.toAmount // without fee

      val fees = exchange.fees

      // Compute totals on both sides of exchange including fees
      var totalSoldAmount = soldAmount
      var totalBoughtAmount = boughtAmount
      for(fee <- fees) {
        if(fee.amount == 0) {
          ;
        } else if(fee.currency == soldCurrency)
          totalSoldAmount += fee.amount
        else if(fee.currency == boughtCurrency)
          totalBoughtAmount += fee.amount
      }

      if(totalSoldAmount==0)
        Logger.warning(s"No sold currencies in this exchange $exchange.")
      if(totalBoughtAmount==0)
        Logger.warning(s"No bought currencies in this exchange $exchange.")

      // Fees expressed in `boughtCurrency' are considered part of the exchange.
      // Those expressed in `soldCurrency' are not.

      // Compute exchange rates (fee in buy side is included)
      val (boughtSoldExchangeRate, soldBoughtExchangeRate) =
        (totalBoughtAmount / soldAmount, soldAmount / totalBoughtAmount)

      // Total value involved in this exchange, expressed in base currency
      // and proxy used to compute (base currency expressed) prices
      val (totalInBaseCurrency, baseCurrencyProxy, baseCurrencyProxyRate) =
        if(soldCurrency == baseCurrency)
          (soldAmount, baseCurrency, 1.0)  // fees expressed in `soldCurrency' are not included
        else if(boughtCurrency == baseCurrency)
          (totalBoughtAmount, baseCurrency, 1.0) // fees expressed in `bougthCurrency' are included
        else if(Currency.priority(soldCurrency) > Currency.priority(boughtCurrency)) {
          val rate = basis.priceOf(soldCurrency, exchange.date)
          (soldAmount * rate, soldCurrency, rate) // fees expressed in `soldCurrency' are not included
        } else {
          val rate = basis.priceOf(boughtCurrency, exchange.date)
          (totalBoughtAmount * rate, boughtCurrency, rate) // fees expressed in `boughtCurrency' are included
        }

      // Price we sold released currencies at, expressed in base currency
      // fees expressed in `soldCurrency' are not included
      val soldPriceInBaseCurrency = totalInBaseCurrency / soldAmount

      // Price we bought acquired currencies at, expressed in base currency
      // fees expressed in `boughtCurrency' are included
      val boughtBasisPriceInBaseCurrency = totalInBaseCurrency / totalBoughtAmount

      val desc = linkToCurrentOperation("Exchange")

      // Add all bought currencies with their cost basis to our stock of assets.
      // Note that fee in `boughtCurrency' are included as they will be deducted right after as a fee
      if(!boughtBasisPriceInBaseCurrency.isNaN && totalBoughtAmount > 0) {
        state.allStocks.add(boughtCurrency, totalBoughtAmount, boughtBasisPriceInBaseCurrency, exchange.exchanger, exchange.date, soldBoughtExchangeRate, soldCurrency, desc, operationNumber)(baseCurrency)
        exchange.exchanger.ledgerPool.record(boughtCurrency)(exchange.date, totalBoughtAmount, exchange.exchanger, desc)
      }

      // Get cost basis for sold currencies from current stock
      // fees expressed in `soldCurrency' are not included
      val (soldBasisInBaseCurrency, noBasis, usedStocks) = {
        exchange.exchanger.ledgerPool.record(soldCurrency)(exchange.date, -soldAmount, exchange.exchanger, desc)

        val t3 = state.allStocks.remove(soldCurrency, soldAmount)(baseCurrency)(exchange.date, exchange.exchanger, desc)
        if(soldCurrency != baseCurrency)
          t3
        else
        // If our base currency is Euro, cost basis is the amount of Euros sold.
        // Try to remove those currencies from our stock of Euros, but basis is
        // the amount sold, even if they are not in our stock
          (soldAmount, 0.0, t3._3)
      }

      // Value of sold currencies, expressed in base currency
      // fees expressed in `soldCurrency' are not included
      val proceedsInBaseCurrency =
        if(soldAmount==0)
          0
        else
          totalInBaseCurrency


      // Gain in this exchange, expressed in base currency.
      // Negative in case it's a loss
      val gainInBaseCurrency = proceedsInBaseCurrency - soldBasisInBaseCurrency


      // Update total gains/looses for soldCurrency
      if(gainInBaseCurrency > 0)
        Realized.perCurrencyGains.record(soldCurrency, gainInBaseCurrency)
      else if(gainInBaseCurrency < 0)
        Realized.perCurrencyLooses.record(soldCurrency, gainInBaseCurrency.abs)


      // If your base currency is Euros, selling Euros is neither a gain nor a loss.
      // In this case proceedsInBaseCurrency will be equal to soldBasisInBaseCurrency.
      // We can add soldBasisInBaseCurrency to our list of costBasis and
      // proceedsInBaseCurrency to our list of proceeds or we can omit both of them.
      // In both cases, net gains will be the same as local result of this exchange
      // will be 0. If `addBaseCurrencySells' is true we take the first approach and add both.
      val addBaseCurrencySells = true

      // Update realized cost basis and proceeds
      if(soldCurrency != baseCurrency || addBaseCurrencySells) {
        Realized.costBasis.record(soldCurrency, soldBasisInBaseCurrency)
        Realized.proceeds.record(soldCurrency, proceedsInBaseCurrency)

        operationTracker.recordCostBasis(operationNumber, soldBasisInBaseCurrency)
        operationTracker.recordProceeds(operationNumber, proceedsInBaseCurrency)
      }


      if(soldCurrency != baseCurrency && soldBasisInBaseCurrency == 0)
        frees += exchange // These assets were acquired for free

      if(soldCurrency != baseCurrency && noBasis.abs > 0.01)
      // Part of these assets were acquired for free
        partiallyFrees += f"Was SOLD but were partially free $noBasis%.8f of $totalSoldAmount%.6f $soldCurrency = ${noBasis * proceedsInBaseCurrency / soldAmount}%.8f $baseCurrency"


      // processed operations resulting from this exchange
      var processed = List[Processed]()

      // process all fees involved in this operation. Note that processing them
      // removes corresponding amounts from our stock of currencies and records the
      // fee as part of the current operation.
      for(fee <- fees; if fee.amount>0)
        processed :+=
          preprocessFee(Fee(
            date = exchange.date
            , id = exchange.id
            , amount = fee.amount
            , currency = fee.currency
            , exchanger = exchange.exchanger
            , description = RichText("")
            , alt = fee.alt
          ))


      val processedExchange =
        Processed.Exchange(
          operationNumber = operationNumber
          , exchange = exchange
          , baseCurrencyProxy = baseCurrencyProxy
          , baseCurrencyProxyRate = baseCurrencyProxyRate
          , boughtBasisPriceInBaseCurrency = boughtBasisPriceInBaseCurrency
          , soldPriceInBaseCurrency = soldPriceInBaseCurrency
          , proceedsInBaseCurrency = proceedsInBaseCurrency
          , soldBasisInBaseCurrency = soldBasisInBaseCurrency
          , _deprecated_feeAmount = 0 // Not useful anymore for _preprocessExchange
          , _deprecated_feeCurrency = "" // Not useful anymore for _preprocessExchange
          , _deprecated_feeInBaseCurrency = 0 // feeInBaseCurrency. Not useful anymore for _preprocessExchange
          , gainInBaseCurrency = gainInBaseCurrency
          , boughtSoldExchangeRate = boughtSoldExchangeRate
          , soldBoughtExchangeRate = soldBoughtExchangeRate
          , usedStocks = usedStocks
          , buys = state.allStocks(boughtCurrency)(baseCurrency).copy
          , sells = state.allStocks(soldCurrency)(baseCurrency).copy
        )


      // A settlement is bought to pay some dues so this is like a buy followed
      // by a loss but
      if(exchange.isSettlement && exchange.exchanger == Poloniex) {
        val currencyKey = marginPairKey(exchange.toCurrency, exchange.fromCurrency)

        val stockContainer = Poloniex.marginShorts(currencyKey)(baseCurrency)
        if(stockContainer.nonEmpty) {
          stockContainer.removeAndGetBasis(totalBoughtAmount)(exchange.date, exchange.exchanger, linkToCurrentOperation(exchange.description.str))
          // We bought these to pay for fees and looses of a short that went against us.
          // We need to clear remaining margin sells
        }
        val processedLoss = preprocessLoss(
          Loss(
            date = exchange.date
            , id = exchange.id
            , amount = totalBoughtAmount
            , currency = exchange.toCurrency
            , exchanger = exchange.exchanger
            , description = RichText("")
          ))
        processed ::= processedLoss
      }

      processed ::= processedExchange // order for a Composed is important. Exchange must be at front
      return simplify(processed)
    }


    def preprocessFee(fee: Fee): Processed.Fee = {
      val desc = linkToCurrentOperation("Fee")
      fee.exchanger.ledgerPool.record(fee.currency)(fee.date, -fee.amount, fee.exchanger, desc)

      val (feeInBaseCurrency0, _, usedStocks) = state.allStocks.remove(fee.currency, fee.amount)(baseCurrency)(fee.date, fee.exchanger, desc)

      val feeInBaseCurrency = if(fee.currency == baseCurrency) fee.amount else feeInBaseCurrency0

      // Record paid fees
      Realized.perCurrencyPaidFees.record(fee.currency, feeInBaseCurrency)

      operationTracker.recordFee(operationNumber, feeInBaseCurrency)

      return Processed.Fee(
        operationNumber = operationNumber
        , fee = fee
        , feeInBaseCurrency = feeInBaseCurrency
        , usedStocks = usedStocks
        , stocks = state.allStocks(fee.currency)(baseCurrency).copy
      )
    }


    def preprocessLoss(loss: Loss): Processed.Loss = {
      val desc = linkToCurrentOperation("Loss")

      loss.exchanger.ledgerPool.record(loss.currency)(loss.date, -loss.amount, loss.exchanger, desc)

      val (lossBasisInBaseCurrency0, _, usedStocks) = state.allStocks.remove(loss.currency, loss.amount)(baseCurrency)(loss.date, loss.exchanger, desc)

      val lossBasisInBaseCurrency = if(loss.currency == baseCurrency) loss.amount else lossBasisInBaseCurrency0

      // Record looses for loss currency
      Realized.perCurrencyLooses.record(loss.currency, lossBasisInBaseCurrency)

      // Record cost bases of lost currencies
      Realized.costBasis.record(loss.currency, lossBasisInBaseCurrency)

      operationTracker.recordCostBasis(operationNumber, lossBasisInBaseCurrency)

      return Processed.Loss(
        operationNumber = operationNumber
        , loss = loss
        , lossInBaseCurrency = lossBasisInBaseCurrency
        , usedStocks = usedStocks
        , stocks = state.allStocks(loss.currency)(baseCurrency).copy
      )
    }


    def preprocessGain(gain: Gain): Processed.Gain = {
      val desc = linkToCurrentOperation("Gain")

      // Record cost basis of gained currencies at price corresponding to gain date
      val basePrice = basis.priceOf(gain.currency, gain.date)
      state.allStocks.add(gain.currency, gain.amount, basePrice, gain.exchanger, gain.date, basePrice, baseCurrency, desc, operationNumber)(baseCurrency)
      gain.exchanger.ledgerPool.record(gain.currency)(gain.date, gain.amount, gain.exchanger, desc)

      val gainInBaseCurrency = gain.amount * basePrice

      // Update total gains for corresponding currency
      Realized.perCurrencyGains.record(gain.currency, gainInBaseCurrency)

      // Make the gain accountable now (by considering it as a proceed)
      Realized.proceeds.record(gain.currency, gainInBaseCurrency)

      operationTracker.recordProceeds(operationNumber, gainInBaseCurrency)

      return Processed.Gain(
        operationNumber = operationNumber
        , gain = gain
        , gainInBaseCurrency = gainInBaseCurrency
        , basePrice = basePrice
        , stocks = state.allStocks(gain.currency)(baseCurrency).copy
      )
    }


    def preprocessMargin(margin: Margin): Seq[Processed] = {
      val soldCurrency = margin.fromCurrency
      val soldAmount = margin.fromAmount

      val boughtCurrency = margin.toCurrency
      val boughtAmount = margin.toAmount

      val fees = margin.fees

      // gain/looses are expressed in marginBaseCurrency
      val (tradedCurrency, marginBaseCurrency) = margin.pair

      val currencyKey = marginPairKey(tradedCurrency, marginBaseCurrency)

      // Check that currencies involved in margin operation are those in traded pair
      if(margin.orderType == Operation.OrderType.Sell) {
        if(soldCurrency != tradedCurrency || boughtCurrency != marginBaseCurrency)
          Logger.fatal(s"Wrong currencies in margin sell: $margin")
      } else if(margin.orderType == Operation.OrderType.Buy) {
        if(soldCurrency != marginBaseCurrency || boughtCurrency != tradedCurrency)
          Logger.fatal(s"Wrong currencies in margin buy: $margin")
      }

      // Compute margin exchange rates
      val (boughtSoldExchangeRate, soldBoughtExchangeRate) =
        (boughtAmount / soldAmount, soldAmount / boughtAmount)

      def processFees(fees: Seq[FeePair], poloniexConversion: Boolean): List[Processed] = {
        // The fee for this operation, expressed in margin base currency. This is only for Poloniex
        var feeInMarginBaseCurrency = 0.0
        if(poloniexConversion)
          for(fee <- fees) {
            if(fee.amount == 0) {
              ;
            } else if(fee.currency == marginBaseCurrency)
              feeInMarginBaseCurrency += fee.amount
            else if(fee.currency == boughtCurrency && soldCurrency == marginBaseCurrency)
              feeInMarginBaseCurrency += soldBoughtExchangeRate * fee.amount
            else if(fee.currency == soldCurrency && boughtCurrency == marginBaseCurrency)
              feeInMarginBaseCurrency += boughtSoldExchangeRate * fee.amount
            else {
              ;
            }
          }

        var processedFees = List[Processed]()
        if(poloniexConversion && margin.exchanger == Poloniex && feeInMarginBaseCurrency > 0) {
          processedFees ::= preprocessFee(
            Fee(margin.date
            , margin.id
            , feeInMarginBaseCurrency
            , marginBaseCurrency
            , margin.exchanger
            , RichText("")
            , Some(feeInMarginBaseCurrency * boughtSoldExchangeRate, boughtCurrency)
            )
          )
        } else {
          for(fee <- fees; if fee.amount > 0)
            processedFees :+= preprocessFee(
              Fee(margin.date
              , margin.id
              , fee.amount
              , fee.currency
              , margin.exchanger
              , RichText("")
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
        marginShorts.add(currencyKey, soldCurrency, soldAmount, boughtSoldExchangeRate, margin.exchanger, margin.date, boughtSoldExchangeRate, boughtCurrency, linkToCurrentOperation("Open short"), operationNumber)(marginBaseCurrency)

        var processed = List[Processed]()

        processed ::=
          Processed.Margin(
            operationNumber = operationNumber
            , date = margin.date
            , exchanger = margin.exchanger
            , what = "Open short"
            , fromAmount = soldAmount, fromCurrency = soldCurrency
            , toAmount = boughtAmount, toCurrency = boughtCurrency
            , exchangeRate = boughtSoldExchangeRate
            , description = margin.description
            , usedStocksOpt = None
            , marginLongs = marginLongs(currencyKey)(marginBaseCurrency).copy
            , marginShorts = marginShorts(currencyKey)(marginBaseCurrency).copy
          )

        processed :::= processFees(fees, poloniexConversion = false)

        return processed
      }

      def openLong(soldAmount: Double, boughtAmount: Double, fees: Seq[FeePair]): List[Processed] = {
        // Opening a long
        marginLongs.add(currencyKey, boughtCurrency, boughtAmount, soldBoughtExchangeRate, margin.exchanger, margin.date, soldBoughtExchangeRate, soldCurrency, linkToCurrentOperation("Open long"), operationNumber)(marginBaseCurrency)

        var processed = List[Processed]()

        processed ::=
          Processed.Margin(
            operationNumber = operationNumber
            , date = margin.date
            , exchanger = margin.exchanger
            , what = "Open long"
            , fromAmount = soldAmount, fromCurrency = soldCurrency
            , toAmount = boughtAmount, toCurrency = boughtCurrency
            , exchangeRate = soldBoughtExchangeRate
            , description = margin.description
            , usedStocksOpt = None
            , marginLongs = marginLongs(currencyKey)(marginBaseCurrency).copy
            , marginShorts = marginShorts(currencyKey)(marginBaseCurrency).copy
          )

        processed :::= processFees(fees, poloniexConversion = true)

        return processed
      }

      if(margin.orderType == Operation.OrderType.Sell) {
        val inLongsAmount = marginLongs(currencyKey)(marginBaseCurrency).totalAmount

        if(inLongsAmount<=0) {
          // Opening a short
          processed :::= openShort(soldAmount, boughtAmount, fees)
        } else {
          // Closing a long
          val closedSoldAmount = soldAmount min inLongsAmount
          val closedBoughtAmount = closedSoldAmount * boughtSoldExchangeRate

          val (basis, noBasis, usedStocks) = marginLongs.remove(currencyKey, soldAmount)(baseCurrency)(margin.date, margin.exchanger, linkToCurrentOperation("Close long"))

          processed ::=
            Processed.Margin(
              operationNumber = operationNumber
              , date = margin.date
              , exchanger = margin.exchanger
              , what = "Close long"
              , fromAmount = closedSoldAmount, fromCurrency = soldCurrency
              , toAmount = closedBoughtAmount, toCurrency = boughtCurrency
              , exchangeRate = boughtSoldExchangeRate
              , description = margin.description
              , usedStocksOpt = Some(usedStocks)
              , marginLongs = marginLongs(currencyKey)(marginBaseCurrency).copy
              , marginShorts = marginShorts(currencyKey)(marginBaseCurrency).copy
            )

          val gain = closedBoughtAmount - basis

          val gainLoss =
            if(gain>0)
              preprocessGain(Gain(margin.date, margin.id, gain, marginBaseCurrency, margin.exchanger, RichText("")))
            else
              preprocessLoss(Loss(margin.date, margin.id, gain.abs, marginBaseCurrency, margin.exchanger, RichText("")))

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
        val inShortsAmount = marginShorts(currencyKey)(marginBaseCurrency).totalAmount

        if(inShortsAmount<=0) {
          // Opening a long
          processed :::= openLong(soldAmount, boughtAmount, fees)
        } else {
          // Closing a short
          val closedBoughtAmount = boughtAmount min inShortsAmount
          val closedSoldAmount = closedBoughtAmount * soldBoughtExchangeRate

          val (basis, noBasis, usedStocks) = marginShorts.remove(currencyKey, boughtAmount)(baseCurrency)(margin.date, margin.exchanger, linkToCurrentOperation("Close short"))

          processed ::=
            Processed.Margin(
              operationNumber = operationNumber
              , date = margin.date
              , exchanger = margin.exchanger
              , what = "Close short"
              , fromAmount = closedSoldAmount, fromCurrency = soldCurrency
              , toAmount = closedBoughtAmount, toCurrency = boughtCurrency
              , exchangeRate = soldBoughtExchangeRate
              , description = margin.description
              , usedStocksOpt = Some(usedStocks)
              , marginLongs = marginLongs(currencyKey)(marginBaseCurrency).copy
              , marginShorts = marginShorts(currencyKey)(marginBaseCurrency).copy
            )

          val gain = basis - closedSoldAmount

          val gainLoss =
            if(gain>0)
              preprocessGain(Gain(margin.date, margin.id, gain, marginBaseCurrency, margin.exchanger, RichText("")) /*, -closedSoldAmount*/)
            else
              preprocessLoss(Loss(margin.date, margin.id, gain.abs, marginBaseCurrency, margin.exchanger, RichText("")))

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
          operationTracker.setDescription(operationNumber, s"Exchange of ${Format.asCurrency(exchange.fromAmount, exchange.fromCurrency)} for ${Format.asCurrency(exchange.toAmount, exchange.toCurrency)}")
          processedOperations += preprocessExchange(exchange)

        case gain: Gain =>
          operationTracker.setDescription(operationNumber, s"Gain of ${Format.asCurrency(gain.amount, gain.currency)}")
          processedOperations += preprocessGain(gain)

        case loss: Loss =>
          operationTracker.setDescription(operationNumber, s"Loss of ${Format.asCurrency(loss.amount, loss.currency)}")
          processedOperations += preprocessLoss(loss)

        case fee: Fee =>
          operationTracker.setDescription(operationNumber, s"Fee of ${Format.asCurrency(fee.amount, fee.currency)}")
          processedOperations += preprocessFee(fee)

        case margin: Margin =>
          val format =
            if(margin.orderType == Operation.OrderType.Buy)
              "Margin buy of %s with %s"
            else
              "Margin sell of %s for %s"
          operationTracker.setDescription(operationNumber, format.format(Format.asCurrency(margin.fromAmount, margin.fromCurrency), Format.asCurrency(margin.toAmount, margin.toCurrency)))
          for(processed <- preprocessMargin(margin))
            processedOperations += processed

        case deposit: Deposit =>
          deposit.exchanger.ledgerPool.record(deposit.currency)(deposit.date, deposit.amount, deposit.exchanger, deposit.description)

        case withdrawal: Withdrawal =>
          withdrawal.exchanger.ledgerPool.record(withdrawal.currency)(withdrawal.date, -withdrawal.amount, withdrawal.exchanger, withdrawal.description)

        case nonTaxableFee: NonTaxableFee =>
          nonTaxableFee.exchanger.ledgerPool.record(nonTaxableFee.currency)(nonTaxableFee.date, -nonTaxableFee.amount, nonTaxableFee.exchanger, nonTaxableFee.description)
          // should be here but breaks back compatibility
          // state.stocks.remove(nonTaxableFee.currency, nonTaxableFee.amount)(baseCurrency)(nonTaxableFee.date, nonTaxableFee.exchanger, nonTaxableFee.description)


      }
    }

    def _deprecated_preprocessExchange(exchange: Exchange): Processed = {
      val soldCurrency = exchange.fromCurrency
      val soldAmount = exchange.fromAmount // without fee

      val boughtCurrency = exchange.toCurrency
      val boughtAmount = exchange.toAmount // without fee

      // extract fee and detached fee from fees list
      val (xs, ys) =
        exchange.fees.filter(fee => fee.amount>0)
        .partition(fee => Set(soldCurrency, boughtCurrency).contains(fee.currency))

      val (feeAmount, feeCurrency, zs) = xs match {
        case Seq() =>
          (0.0, boughtCurrency, ys)
        case Seq(fee, fees @ _*) =>
          (fee.amount, fee.currency, fees ++ ys)
      }

      val detachedFeeOpt = zs match {
        case Seq() =>
          None
        case Seq(fee, fees @ _*) =>
          Some(FeePair(fee.amount, fee.currency))
      }

      val (totalSoldAmount, totalBoughtAmount) = // including fee
        if(feeAmount == 0)
          (soldAmount, boughtAmount)
        else if(feeCurrency == soldCurrency)
          (soldAmount + feeAmount, boughtAmount)
        else if(feeCurrency == boughtCurrency)
          (soldAmount, boughtAmount + feeAmount)
        else
          Logger.fatal(s"Cannot process exchange as fee is not expressed in same unit as from or to currencies.${exchange.toString}")


      if(totalSoldAmount==0)
        Logger.warning(s"No sold currencies in this exchange $exchange.")
      if(totalBoughtAmount==0)
        Logger.warning(s"No bought currencies in this exchange $exchange.")


      // Compute exchange rates (fee is included)
      val (boughtSoldExchangeRate, soldBoughtExchangeRate) =
        (totalBoughtAmount / totalSoldAmount, totalSoldAmount / totalBoughtAmount)

      // Total value involved in this operation, expressed in base currency
      // and proxy used to compute (base currency expressed) prices
      val (totalInBaseCurrency, baseCurrencyProxy, baseCurrencyProxyRate) =
      if(soldCurrency == baseCurrency)
        (totalSoldAmount, baseCurrency, 1.0)
      else if(boughtCurrency == baseCurrency)
        (totalBoughtAmount, baseCurrency, 1.0)
      else if(Currency.priority(soldCurrency) > Currency.priority(boughtCurrency)) {
        val rate = basis.priceOf(soldCurrency, exchange.date)
        (totalSoldAmount * rate, soldCurrency, rate)
      } else {
        val rate = basis.priceOf(boughtCurrency, exchange.date)
        (totalBoughtAmount * rate, boughtCurrency, rate)
      }

      // fee for this operation, expressed in base currency,
      // only if fee is included in this exchange
      val feeInBaseCurrency =
        if(feeAmount == 0) // we don't care about feecurrency in this case
          0
        else if(feeCurrency == baseCurrency)
          feeAmount
        else if(feeCurrency == boughtCurrency && soldCurrency == baseCurrency)
          soldBoughtExchangeRate * feeAmount
        else if(feeCurrency == soldCurrency && boughtCurrency == baseCurrency)
          boughtSoldExchangeRate * feeAmount
        else if(feeCurrency == boughtCurrency)
          feeAmount * totalInBaseCurrency / totalBoughtAmount
        else if(feeCurrency == soldCurrency)
          feeAmount * totalInBaseCurrency / totalSoldAmount
        else
          Logger.fatal(s"Cannot process exchange as fee is not expressed in same unit as from or to currencies.${exchange.toString}")

      // Price we sold released currencies at, expressed in base currency
      val soldPriceInBaseCurrency = totalInBaseCurrency / totalSoldAmount

      // Price we bought acquired currencies at, expressed in base currency
      val boughtBasisPriceInBaseCurrency = totalInBaseCurrency / totalBoughtAmount

      val desc = linkToCurrentOperation("Exchange")

      // Add bought currencies (without paid fee) with their cost basis to our stock of assets
      if(!boughtBasisPriceInBaseCurrency.isNaN && boughtAmount > 0) {
        state.allStocks.add(boughtCurrency, boughtAmount, boughtBasisPriceInBaseCurrency, exchange.exchanger, exchange.date, soldBoughtExchangeRate, soldCurrency, desc, operationNumber)(baseCurrency)
        exchange.exchanger.ledgerPool.record(boughtCurrency)(exchange.date, boughtAmount, exchange.exchanger, desc)
      }

      // Get cost basis for total sold currencies from current stock
      val (totalSoldBasisInBaseCurrency, noBasis, usedStocks) =
        if(soldCurrency != baseCurrency) {
          exchange.exchanger.ledgerPool.record(soldCurrency)(exchange.date, -totalSoldAmount, exchange.exchanger, desc)
          state.allStocks.remove(soldCurrency, totalSoldAmount)(baseCurrency)(exchange.date, exchange.exchanger, desc)
        } else {
          // If our base currency is Euro, cost basis is the amount of Euros involved
          exchange.exchanger.ledgerPool.record(soldCurrency)(exchange.date, -totalSoldAmount, exchange.exchanger, desc)
          val (_,_,usedStocks) = state.allStocks.remove(soldCurrency, totalSoldAmount)(baseCurrency)(exchange.date, exchange.exchanger, desc)
          (totalSoldAmount, 0.0, usedStocks)
        }

      // Value of sold currencies (without fee), expressed in base currency
      val proceedsInBaseCurrency =
        if(soldAmount==0)
          0
        else
          soldPriceInBaseCurrency * soldAmount

      // Cost basis of sold currencies (without fee), expressed in base currency
      val soldBasisInBaseCurrency =
        if(totalSoldAmount==0)
          0
        else
          totalSoldBasisInBaseCurrency * soldAmount / totalSoldAmount

      // Gain in this exchange (without fee), expressed in base currency
      val gainInBaseCurrency =
        if(soldCurrency == baseCurrency) // If base currency is Euros, releasing Euros makes no profit
          0
        else
          proceedsInBaseCurrency - soldBasisInBaseCurrency

      // Update paid fees
      Realized.perCurrencyPaidFees.record(feeCurrency, feeInBaseCurrency)

      // Update total gains for soldCurrency
      if(gainInBaseCurrency > 0)
        Realized.perCurrencyGains.record(soldCurrency, gainInBaseCurrency)
      else if(gainInBaseCurrency < 0)
        Realized.perCurrencyLooses.record(soldCurrency, gainInBaseCurrency.abs)

      // Update realized cost basis and proceeds
      if(soldCurrency == baseCurrency) {
        // this is the case for Euro -> Any Thing if your base is Euro.
        ;
      } else {
        Realized.costBasis.record(soldCurrency, soldBasisInBaseCurrency)
        Realized.proceeds.record(soldCurrency, proceedsInBaseCurrency)
      }

      if(totalSoldBasisInBaseCurrency == 0 && (soldCurrency != baseCurrency))
        frees += exchange // These assets were acquired for free

      if(noBasis.abs > 0.01 && (soldCurrency != baseCurrency))
      // Part of these assets were acquired for free
        partiallyFrees += f"Was SOLD but were partially free $noBasis%.8f of $totalSoldAmount%.6f $soldCurrency = ${noBasis * proceedsInBaseCurrency / soldAmount}%.8f $baseCurrency"

      operationTracker.recordFee(operationNumber, feeInBaseCurrency)

      if(soldCurrency == baseCurrency) {
        ;
      } else {
        operationTracker.recordCostBasis(operationNumber, soldBasisInBaseCurrency)
        operationTracker.recordProceeds(operationNumber, proceedsInBaseCurrency)
      }

      val processedExchange =
        Processed.Exchange(
          operationNumber = operationNumber
          , exchange = exchange
          , baseCurrencyProxy = baseCurrencyProxy
          , baseCurrencyProxyRate = baseCurrencyProxyRate
          , boughtBasisPriceInBaseCurrency = boughtBasisPriceInBaseCurrency
          , soldPriceInBaseCurrency = soldPriceInBaseCurrency
          , proceedsInBaseCurrency = proceedsInBaseCurrency
          , soldBasisInBaseCurrency = soldBasisInBaseCurrency
          , _deprecated_feeAmount = feeAmount
          , _deprecated_feeCurrency = feeCurrency
          , _deprecated_feeInBaseCurrency = feeInBaseCurrency
          , gainInBaseCurrency = gainInBaseCurrency
          , boughtSoldExchangeRate = boughtSoldExchangeRate
          , soldBoughtExchangeRate = soldBoughtExchangeRate
          , usedStocks = usedStocks
          , buys = state.allStocks(boughtCurrency)(baseCurrency).copy
          , sells = state.allStocks(soldCurrency)(baseCurrency).copy
        )

      var processed = List[Processed]()

      // A settlement is bought to pay some dues so this is like a buy followed
      // by a loss but
      if(exchange.isSettlement && exchange.exchanger == Poloniex) {
        val currencyKey = marginPairKey(exchange.toCurrency, exchange.fromCurrency)

        val stockContainer = Poloniex.marginShorts(currencyKey)(baseCurrency)
        if(stockContainer.nonEmpty) {
          stockContainer.removeAndGetBasis(totalBoughtAmount)(exchange.date, exchange.exchanger, linkToCurrentOperation(exchange.description.str))
          // We bought these to pay for fees and looses of a short that went against us.
          // We need to clear remaining margin sells
        }
        val processedLoss = preprocessLoss(
          Loss(
            date = exchange.date
            , id = exchange.id
            , amount = totalBoughtAmount
            , currency = exchange.toCurrency
            , exchanger = exchange.exchanger
            , description = RichText("")
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
              , currency = fee.currency
              , exchanger = exchange.exchanger
              , description = RichText("")
            ))
          processed ::= processedFee
      }

      processed ::= processedExchange // order for a Composed is important. Exchange must be at front
      return simplify(processed)
    }

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

      val m = state.allStocks(Currency.bitcoin)(baseCurrency).totalAmount
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
