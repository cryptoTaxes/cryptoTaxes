package taxes

import java.io.PrintStream
import java.math.RoundingMode
import java.text.{DecimalFormat, NumberFormat, SimpleDateFormat}

import taxes.Exchanger._
import taxes.Market.Market
import taxes.Util.Logger

import scala.xml.Elem


object Format {
  val df = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")

  val shortDf = new SimpleDateFormat("yyyy-MM-dd")

  val decimalSep = java.text.NumberFormat.getInstance().asInstanceOf[DecimalFormat].getDecimalFormatSymbols.getDecimalSeparator

  def trimZeros(str0 : String) : String = {
    var str = str0.reverse
    if(str0.contains(decimalSep)) {
      str = str.dropWhile(_ == '0')
      if (str.nonEmpty && str.head == decimalSep)
        str = str.tail
    }
    return str.reverse
  }

  def formatDecimal(x : Double, decimals : Int = 2) : String = {
    val xAbs = x.abs
    var fmt =
      if (xAbs < 0.00000009)
        "0.000000000"
      else if (xAbs < 0.0000009)
        "0.00000000"
      else if (xAbs < 0.000009)
        "0.0000000"
      else if (xAbs < 0.00009)
        "0.000000"
      else if (xAbs < 0.0009)
        "0.00000"
      else if (xAbs < 0.009)
        "0.0000"
      else if (xAbs < 0.09)
        "0.000"
      else
        "0.00"

    while(fmt.length - 2 < decimals)
      fmt = fmt + '0'

    val df = new DecimalFormat(fmt)
    df.setRoundingMode(RoundingMode.DOWN)
    return trimZeros(df.format(x))
  }

  def marketFormat(marketUnit : Market) : NumberFormat =
    if(marketUnit == Market.euro)
      EuroBaseCoin.format
    else if(marketUnit == Market.usd)
      USDBaseCoin.format
    else
      BTCBaseCoin.format

  def asMarket(amount : Double, marketUnit : Market, decimals : Int = 2) : String =
    Format.formatDecimal(amount, decimals) + " " + marketUnit
}


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

  override def toHTML : Elem =
    toHTML()

  def toHTML(caption : String = "") : Elem =
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
           <td>{HTML.asMarket(total, baseMarket)}</td>
           <td>{HTML.asMarket(sum, baseMarket)}</td>
        </tr>
      }
      }
      <td class='embold'>Total:</td>
      <td></td>
      <td>{HTML.asMarket(map.values.sum, baseMarket)}</td>
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

  def toCSVFile(fileName : String, year : Int): Unit = {
    val csvFileName = Paths.userOutputFolder+"/%s%d.csv".format(fileName,year)
    val ps = new PrintStream(csvFileName)

    ps.println
    ps.println("FIFO %d".format(year))
    ps.println("")

    val sep = ";"
    val header = List("order", "date", "exchanger", "description", "cost basis/loss", "proceeds/gain", "fee")

    ps.println(header.mkString(";"))
    for((operationNumber,entry) <- this)
      ps.println(List[Any](operationNumber, Format.shortDf.format(entry.date), entry.exchanger, entry.description, entry.costBasis, entry.proceeds, entry.fee).mkString(";"))

    ps.close()
  }

}


object FIFO {
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

    var processedOperations = List[Processed]()


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

    // Sold stocks without corresponding buys
    var frees = List[String]()
    var price0 = List[Exchange]()

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

      frees = List[String]()
      price0 = List[Exchange]()

      operationNumber = 0

      processedOperations = List[Processed]()

      operationTracker.clear()
      return year
    }


    def reportYear(year : Int): Unit = {
      val htmlFIFOFile = Paths.userOutputFolder+"/FIFO%d.html".format(year)
      val htmlFIFOTitle = "%d FIFO Report".format(year)
      val htmlFIFO = HTML(htmlFIFOFile, htmlFIFOTitle)

      htmlFIFO += <div class='header'>{htmlFIFOTitle}</div>

      for(processed <- processedOperations.reverse)
        htmlFIFO += processed

      htmlFIFO += HTML.reportResults(year, Realized)
      htmlFIFO.close()


      val htmlPortfolioFile = Paths.userOutputFolder+"/Portfolio%d.html".format(year)
      val htmlPortfolioTitle = "%d End of year portfolio".format(year)
      val htmlPortfolio = HTML(htmlPortfolioFile, htmlPortfolioTitle)

      htmlPortfolio += stocks.toHTML(htmlPortfolioTitle)
      htmlPortfolio.close()


      val htmlExtraFile = Paths.userOutputFolder+"/Extra%d.html".format(year)
      val htmlExtraTitle = "%d Statistics".format(year)
      val htmlExtra = HTML(htmlExtraFile, htmlExtraTitle)

      htmlExtra += <div class='header'>{htmlExtraTitle}</div>
      htmlExtra += HTML.reportYear(year, Realized)

      if(Config.verbosity(Verbosity.showMoreDetails)) {
        {htmlExtra += <div>Frees:</div>}
        <div>
        {for(f <- frees.reverse)
          htmlExtra += <div>{f}</div>
        }
        </div>

        {htmlExtra += <div>Priced0:</div>}
        <div>
          {for(op <- price0.reverse)
            htmlExtra += <div>{op}</div>
          }
        </div>

        {htmlExtra += <div>Opened margin longs:</div>}
        <div>
          {for(marginBuy <- marginBuysMap.values)
            for (cont <- marginBuy)
              if(cont.totalAmount>0)
                htmlExtra += cont.toHTML(showTotal = true)
          }
        </div>

        {htmlExtra += <div>Opened margin shorts:</div>}
        <div>
          {for(marginSell <- marginSellsMap.values)
          for (cont <- marginSell)
            if(cont.totalAmount>0)
              htmlExtra += cont.toHTML(showTotal = true)
          }
        </div>
      }
      htmlExtra.close()

      operationTracker.toCSVFile("FIFO", year)
    }


    def preprocessExchange(exchange: Exchange) : Processed.Exchange = {
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
      val (soldBasisInBaseCoin, noBasis, usedStocks) =
        if (soldMarket != baseMarket)
          stocks.remove(soldMarket, soldAmount)
        else {
          val (_,_,usedStocks) = stocks.remove(soldMarket, soldAmount)
          (soldAmount, 0.0, usedStocks)
        }

      // Total value of sold coins, expressed in base coin
      val proceedsInBaseCoin = totalInBaseCoin

      // Gain in this exchange, expressed in base coin
      val gainInBaseCoin =
        if (soldMarket == baseMarket) // If base coin is Euros, buying with euros makes no profit
          0
        else
          proceedsInBaseCoin - soldBasisInBaseCoin

      Realized.perMarketPaidFees.record(exchange.feeMarket, feeInBaseCoin)

      // Update total gains for soldMarket
      if (gainInBaseCoin > 0)
        Realized.perMarketGains.record(soldMarket, gainInBaseCoin)
      else if (gainInBaseCoin < 0)
        Realized.perMarketLooses.record(soldMarket, gainInBaseCoin.abs)

      // Update realized cost basis and proceeds
      if(soldMarket == baseMarket) {
        // toDo this is the case for Euro -> BTC if your base is Euro.
        ;
      } else {
        Realized.costBasis.record(soldMarket, soldBasisInBaseCoin)
        Realized.proceeds.record(soldMarket, proceedsInBaseCoin)
      }

      if (soldBasisInBaseCoin == 0 && (soldMarket != baseMarket))
        price0 ::= exchange

      if (noBasis.abs > 0.01 && (soldMarket != baseMarket))
      //toDO fix me
        frees ::= "SOLD free %.8f of %.6f %s = %.8f %s  ".format(noBasis, soldAmount, soldMarket, noBasis * proceedsInBaseCoin / soldAmount, baseMarket)

      operationTracker.recordFee(operationNumber, feeInBaseCoin)
      if(soldMarket == baseMarket) {
        ;
      } else {
        operationTracker.recordCostBasis(operationNumber, soldBasisInBaseCoin)
        operationTracker.recordProceeds(operationNumber, proceedsInBaseCoin)
      }

      return Processed.Exchange(
            operationNumber = operationNumber
          , exchange = exchange
          , baseCoinProxy = baseCoinProxy
          , baseCoinProxyRate = baseCoin.priceInBaseCoin(baseCoinProxy, exchange.date)
          , boughtBasisPriceInBaseCoin = boughtBasisPriceInBaseCoin
          , soldPriceInBaseCoin = soldPriceInBaseCoin
          , proceedsInBaseCoin = proceedsInBaseCoin
          , soldBasisInBaseCoin = soldBasisInBaseCoin
          , feeInBaseCoin = feeInBaseCoin
          , gainInBaseCoin = gainInBaseCoin
          , boughtSoldExchangeRate = boughtSoldExchangeRate, soldBoughtExchangeRate = soldBoughtExchangeRate
          , usedStocks = usedStocks
        )
    }


    /*

    def processFee(fee : Fee) {
      val (feeCostInBaseCoin, _, usedStocks) = stocks.remove(fee.market, fee.amount)

      Realized.perMarketPaidFees.record(fee.market, feeCostInBaseCoin)

      // Paying a fee implies loosing paid amount.
      // Update total looses for soldMarket
      Realized.perMarketLooses.record(fee.market, feeCostInBaseCoin)

      // Record cost basis of paid coins
      Realized.costBasis.record(fee.market, feeCostInBaseCoin)

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

      val csvEntry = csv.Entry(
          date = Some(fee.date)
        , sold = Some("Fee")
        , fee = Some(feeCostInBaseCoin)
        , exchanger = fee.exchanger
      )

      csv.println(csvEntry)

      htmlFIFO += HTML.Fee(
        operationNumber = operationNumber
        , fee = fee
        , feeCostInBaseCoin = feeCostInBaseCoin
        , usedStocksFee = usedStocks
      )
    }



    def processLoss(loss : Loss) {
      val (feeBasisInBaseCoin, _, usedStocksFee) = stocks.remove(loss.feeMarket, loss.fee)
      Realized.perMarketPaidFees.record(loss.feeMarket, feeBasisInBaseCoin)

      val (lossBasisInBaseCoin, _, usedStocksLoss) = stocks.remove(loss.market, loss.amount)

      // take fee into account
      val totalLossInBaseCoin = lossBasisInBaseCoin + feeBasisInBaseCoin

      // Update total looses for loss market
      Realized.perMarketLooses.record(loss.market, totalLossInBaseCoin)

      // Record cost bases of lost coins and paid fee
      Realized.costBasis.record(loss.market, lossBasisInBaseCoin)
      Realized.costBasis.record(loss.feeMarket, feeBasisInBaseCoin)

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
          "COST BASIS:".tab(0) + Format.asMarket(lossBasisInBaseCoin, baseMarket)
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
        "LOSS:".tab(0) + Format.asMarket(-lossBasisInBaseCoin, baseMarket)).tab(1)
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
        , costBasis = Some(lossBasisInBaseCoin)
        , fee = Some(feeBasisInBaseCoin)
        , exchanger = loss.exchanger
      )

      csv.println(csvEntry)

      htmlFIFO += HTML.Loss(
        operationNumber = operationNumber
        , loss = loss
        , lossInBaseCoin = lossBasisInBaseCoin
        , usedStocksLoss = usedStocksLoss
        , feeInBaseCoin = feeBasisInBaseCoin
        , usedStocksFee = usedStocksFee
      )
    }

    def processGain(gain : Gain) {
      val (feeBasisInBaseCoin, _, usedStocksFee) = stocks.remove(gain.feeMarket, gain.fee)
      Realized.perMarketPaidFees.record(gain.feeMarket, feeBasisInBaseCoin)

      // Paying a fee implies loosing paid amount
      Realized.perMarketLooses.record(gain.feeMarket, feeBasisInBaseCoin)

      // Record cost basis of gained coins at current date
      val basePrice = baseCoin.priceInBaseCoin(gain.market, gain.date)
      stocks.add(gain.market, gain.amount, basePrice, gain.exchanger, gain.date)

      val gainInBaseCoin = gain.amount*basePrice

      // Update total gains for corresponding market
      Realized.perMarketGains.record(gain.market, gainInBaseCoin)

      // Record cost basis of paid fee and make the gain accountable now (by considering it as a sell)
      Realized.costBasis.record(gain.feeMarket, feeBasisInBaseCoin)
      Realized.proceeds.record(gain.market, gainInBaseCoin)
      // toDo Maybe should only record proceeds as gainInBaseCoin - feeBasisInBaseCoin

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
        , exchanger = gain.exchanger
      )

      csv.println(csvEntry)

      htmlFIFO += HTML.Gain(
        operationNumber = operationNumber
        , gain = gain
        , gainInBaseCoin = gainInBaseCoin
        , feeInBaseCoin = feeBasisInBaseCoin
        , basePrice = basePrice
        , usedStocksFee = usedStocksFee
      )
    }

*/

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


    /*
        def preprocessGain(gain : Gain, costBasis : Double = 0) : Processed.Gain = {
      // Record cost basis of gained coins at current date
      val basePrice = baseCoin.priceInBaseCoin(gain.market, gain.date)
      stocks.add(gain.market, gain.amount, basePrice, gain.exchanger, gain.date)

      val gainInBaseCoin = gain.amount * basePrice

      val basisInBaseCoin = costBasis * basePrice

      // Update total gains for corresponding market
      Realized.perMarketGains.record(gain.market, gainInBaseCoin)

      // Make the gain accountable now (by considering it as a sell)
      Realized.costBasis.record(gain.market, basisInBaseCoin)
      Realized.proceeds.record(gain.market, gainInBaseCoin + basisInBaseCoin)
      // toDo Maybe should only record proceeds as gainInBaseCoin - feeBasisInBaseCoin


      val csvEntry = csv.Entry(
        date = Some(gain.date)
        , sold = Some("Gain")
        , bought = Some(gain.market)
        , boughtAmount = Some(gain.amount)
        , sellValue = Some(gainInBaseCoin)
        , exchanger = gain.exchanger
      )

      //csv.println(csvEntry)


      if (costBasis < 0) {
        csvTracker.recordCostBasis(operationNumber, -(costBasis + gain.amount) * basePrice)
        csvTracker.recordProceeds(operationNumber, -costBasis * basePrice)
      } else {
        csvTracker.recordCostBasis(operationNumber, costBasis * basePrice)
        csvTracker.recordProceeds(operationNumber, (costBasis + gain.amount) * basePrice)
      }

      return Processed.Gain(
        operationNumber = operationNumber
        , gain = gain
        , gainInBaseCoin = gainInBaseCoin
        , basePrice = basePrice
      )
    }
     */

    def preprocessGain(gain : Gain) : Processed.Gain = {
      // Record cost basis of gained coins at current date
      val basePrice = baseCoin.priceInBaseCoin(gain.market, gain.date)
      stocks.add(gain.market, gain.amount, basePrice, gain.exchanger, gain.date)

      val gainInBaseCoin = gain.amount * basePrice

      // Update total gains for corresponding market
      Realized.perMarketGains.record(gain.market, gainInBaseCoin)

      // Make the gain accountable now (by considering it as a sell)
      Realized.proceeds.record(gain.market, gainInBaseCoin)

      operationTracker.recordProceeds(operationNumber, gainInBaseCoin)

      return Processed.Gain(
        operationNumber = operationNumber
        , gain = gain
        , gainInBaseCoin = gainInBaseCoin
        , basePrice = basePrice
      )
    }


    def marginPairKey(market1 : Market, market2: Market) : String
      = market1 ++ "/" ++ market2

    def preprocessSettlement(settlement: SettlementBuy) : Processed.Composed = {
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

      // This is like a loss but we have to remove bought coins from stock of opened shorts
      // (we bought them to pay for a short that went against us)
      if(settlement.exchanger == Poloniex) {
        val stockContainer = marginSellsMap(Poloniex)(marketKey)
        if (stockContainer.nonEmpty) {
          stockContainer.removeAndGetBasis(boughtAmount)
        }
      }

      val processedSettlement =
        Processed.SettlementBuy(
          operationNumber = operationNumber
          , settlement = settlement
        )

      val loss = preprocessLoss(Loss(settlement.date, settlement.id, totalInTradingBaseCoin, tradingBaseMarket, settlement.exchanger, ""))

      return Processed.Composed(operationNumber, List(processedSettlement, loss))
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

      var processed = List[Processed]()

      var processedAux = List[Processed]()

      def nonZero(x : Double) : Boolean = x.abs >= 1E-7

      def openShort(soldAmount : Double, boughtAmount : Double, feeAmount: Double, feeMarket : Market, feeInTradingBaseCoin : Double) : List[Processed] = {
        // Opening a short
        marginSells.add(marketKey, soldMarket, soldAmount, boughtSoldExchangeRate, margin.exchanger, margin.date)

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
          )

        if(feeInTradingBaseCoin>0) {
          val fee = Fee(margin.date, margin.id, feeInTradingBaseCoin, tradingBaseMarket, margin.exchanger, "")
          processed ::= preprocessFee(fee)
        }
        return processed
      }

      def openLong(soldAmount : Double, boughtAmount : Double, feeAmount: Double, feeMarket : Market, feeInTradingBaseCoin : Double) : List[Processed] = {
        // Opening a long
        marginBuys.add(marketKey, boughtMarket, boughtAmount, soldBoughtExchangeRate, margin.exchanger, margin.date)

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
          )

        if(feeInTradingBaseCoin>0) {
          val fee = Fee(margin.date, margin.id, feeInTradingBaseCoin, tradingBaseMarket, margin.exchanger, "", Some(feeInTradingBaseCoin*boughtSoldExchangeRate, boughtMarket))
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
          val closedBoughtAmount = closedSoldAmount * boughtAmount / soldAmount

          val (basis, noBasis, usedStocks) = marginBuys.remove(marketKey, soldAmount)

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
            )

          val gain = closedBoughtAmount - basis

          if(feeInTradingBaseCoin>0) {
            val fee = Fee(margin.date, margin.id, feeInTradingBaseCoin, tradingBaseMarket, margin.exchanger, "")
            processed ::= preprocessFee(fee)
          }

          processed ::= (
            if(gain>0)
              preprocessGain(Gain(margin.date, margin.id, gain, tradingBaseMarket, margin.exchanger, "") /*, basis*/)
            else
              preprocessLoss(Loss(margin.date, margin.id, gain.abs, tradingBaseMarket, margin.exchanger, ""))
          )

          if(nonZero(noBasis)) {
            // we are also opening a short
            val longedSoldAmount = soldAmount - closedSoldAmount // same as noBasis
            val longedBoughtAmount = boughtAmount - closedBoughtAmount
            processedAux = openShort(longedSoldAmount, longedBoughtAmount, 0, margin.feeMarket, 0)
            // Logger.warning("Shorting No basis > 0: " + (noBasis, longedSoldAmount) + " " + margin)
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
          val closedSoldAmount = closedBoughtAmount * soldAmount / boughtAmount

          val (basis, noBasis, usedStocks) = marginSells.remove(marketKey, boughtAmount)

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
            )

          // val gain = basis - (closedSoldAmount-feeInTradingBaseCoin) - feeInTradingBaseCoin
          val gain = basis - closedSoldAmount

          if(feeInTradingBaseCoin>0) {
            val fee = Fee(margin.date, margin.id, feeInTradingBaseCoin, tradingBaseMarket, margin.exchanger, "", alt = Some(feeInTradingBaseCoin*boughtSoldExchangeRate, boughtMarket))
            processed ::= preprocessFee(fee)
          }

          processed ::= (
            if(gain>0)
              preprocessGain(Gain(margin.date, margin.id, gain, tradingBaseMarket, margin.exchanger, "") /*, -closedSoldAmount*/)
            else
              preprocessLoss(Loss(margin.date, margin.id, gain.abs, tradingBaseMarket, margin.exchanger, ""))
          )

          if(nonZero(noBasis)) {
            // we are also opening a long
            val longedSoldAmount = soldAmount - closedSoldAmount
            val longedBoughtAmount = boughtAmount - closedBoughtAmount // same as noBasis
            processedAux = openLong(longedSoldAmount, longedBoughtAmount, 0, margin.feeMarket, 0)
            // Logger.warning("Longing No basis > 0: " + (noBasis, longedBoughtAmount) + " " + margin)
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
          processedOperations ::= preprocessExchange(exchange)

        case fee : Fee =>
          operationTracker.setDescription(operationNumber, "Fee of %s".format(Format.asMarket(fee.amount, fee.market)))
          processedOperations ::= preprocessFee(fee)

        case settlement : SettlementBuy =>
          operationTracker.setDescription(operationNumber, "Settlement exchange of %s for %s".format(Format.asMarket(settlement.fromAmount, settlement.fromMarket), Format.asMarket(settlement.toAmount, settlement.toMarket)))
          processedOperations ::= preprocessSettlement(settlement)

        case margin : Margin =>
          val format =
            if(margin.orderType == Operation.OrderType.Buy)
              "Margin buy of %s with %s"
            else
              "Margin sell of %s for %s"
          operationTracker.setDescription(operationNumber, format.format(Format.asMarket(margin.fromAmount, margin.fromMarket), Format.asMarket(margin.toAmount, margin.toMarket)))
          for(processed <- preprocessMargin(margin))
            processedOperations ::= processed
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