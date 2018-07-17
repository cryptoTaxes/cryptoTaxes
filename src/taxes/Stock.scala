package taxes

import java.text.SimpleDateFormat

import taxes.date._
import taxes.exchanger.Exchanger

import scala.collection.mutable.ListBuffer


// basis is expressed in base unit. exchanger is where it was bought
case class Stock(var amount : Double, costBasis : Price, exchanger : Exchanger, date : LocalDateTime, exchangeRate : Price, exchangeMarket : Market) {
  override def toString : String =
    "Stock(%.4f, %.8f, %s, %.8f, %s, ".format(amount, costBasis, exchanger, exchangeRate, exchangeMarket) +
    new SimpleDateFormat("yyyy-MM-dd").format(date) +
    ")"
}


trait StockContainer extends Container[Stock] with ToHTML {
  val id : String

  val market : Market  // what do we store

  val baseMarket : Market // what are prices expressed in

  def copy: StockContainer

  val eps : Double =
    if(baseMarket==Market.euro || baseMarket==Market.usd)
      0.00001
    else
      Config.config.epsilon


  case class Ledger(date : LocalDateTime, amount : Double, exchanger: Exchanger, description : String)

  val ledger = ListBuffer[Ledger]()

  def insert(x : Stock, desc : String): Unit = {
    ledger += Ledger(x.date, x.amount, x.exchanger, desc)
    insert(x)
  }

  def insert(x : Stock, eq : (Stock,Stock) => Boolean, combine : (Stock,Stock) => Stock, desc : String): Unit = {
    ledger += Ledger(x.date, x.amount, x.exchanger, desc)
    insert(x, eq, combine)
  }

  def removeAndGetBasis(amount : Double)(date: LocalDateTime, exchanger: Exchanger, desc : String) : (Price, Double, StockContainer) = {
    var toRemove = amount
    var basis = 0.0
    var done = false
    val usedStocks = StockQueue(id, market, baseMarket)
    while(!this.isEmpty && !done) {
      val stock = this.first
      if(stock.amount >= toRemove) {
        basis += toRemove * stock.costBasis
        stock.amount -= toRemove
        usedStocks.insert(stock.copy(amount = toRemove))
        toRemove = 0
        if(stock.amount < eps)
          this.removeFirst()
        done = true
      } else {
        basis += stock.amount * stock.costBasis
        toRemove -= stock.amount
        this.removeFirst()
        usedStocks.insert(stock.copy())
      }
    }
    val removed = amount - toRemove
    ledger += Ledger(date, -removed, exchanger, desc)
    return if(done) (basis, 0, usedStocks) else (basis, toRemove, usedStocks)
  }

  def totalAmount : Double =
    this.iterator.map(_.amount).sum

  def totalCost : Double =
    this.iterator.map(p => p.amount * p.costBasis).sum

  def ledgerToHTML(year : Int) : Option[HTML] =
     {var rows = 0
      var balance = 0.0
      val fmt = "%.8f"

       val table =
        <table id='tableStyle1'>
          <tr>
            <th>Date</th>
            <th>Amount</th>
            <th>Balance</th>
            <th>Exchanger</th>
            <th class='alignL'>Description</th>
          </tr>
          <caption>{this.market}</caption>
            {ledger.map{ l => {
                balance += l.amount
                if(l.date.getYear == year) {
                  rows += 1
                  <tr>
                    <td>{Format.df.format(l.date)}</td>
                    <td class={"paddingL" + (if (l.amount<0) " darkRed" else " darkBlue")}>
                      {fmt.format(l.amount)}
                    </td>
                    <td class='paddingL'>{fmt.format(balance)}</td>
                    <td class='exchanger'>{l.exchanger}</td>
                    <td class='alignL'>{l.description}</td>
                  </tr>
                }
             }}
            }
            {if(rows>0)
                <tr>
                  <th>Final balance:</th>
                  <th></th>
                  <th>{fmt.format(balance)}</th>
                  <th></th>
                  <th></th>
                </tr>

            }
          </table>
         if(rows>0)
           Some{table}
         else
           None
        }

  override def toHTML : HTML = toHTML()

  def toHTML(showTotal : Boolean = true, showAmounts : Boolean = true, showExchangeRates : Boolean = false) : HTML =
    <span>
      {if(showTotal) {
        <span>
          {HTMLDoc.asMarket(totalAmount, market)}.
          {HTMLDoc.asMarket(totalCost, baseMarket)}.
        </span>
       }
      }
      {zipWithIndex.map{ case (stock, i) =>
      <span>
      <span class='noLineBreak'>
        <span>
          {stock.date.format(Format.shortDf)}
        </span>
        <span class='exchanger'>
          {stock.exchanger}
        </span>
        {if(showExchangeRates && stock.exchangeMarket != baseMarket)
          <span>
            {if(showAmounts)
              Format.formatDecimal(stock.amount)+" x"
             else
              ""
            }
            {HTMLDoc.asRate(stock.exchangeRate, stock.exchangeMarket, market)}
            =
          </span>
        }
        <span>
          {if(showAmounts)
            Format.formatDecimal(stock.amount)+" x"
           else
            ""
          }
          {HTMLDoc.asRate(stock.costBasis, baseMarket, market)}
            {if (i < size - 1) "," else ""}
        </span>
      </span>
      </span>
    }}
  </span>
}


case class StockQueue(id : String, market : Market, baseMarket : Market) extends Queue[Stock] with StockContainer {
  override def copy: StockQueue = {
    val clone = StockQueue(id, market, baseMarket)
    for(x <- this)
      clone.insert(x.copy(), "")
    return clone
  }
}


case class StockStack(id : String, market : Market, baseMarket : Market) extends Stack[Stock] with StockContainer {
  override def copy: StockStack = {
    val clone = StockStack(id, market, baseMarket)
    for(x <- this.reversed)
      clone.insert(x.copy(), "")
    return clone
  }
}


trait StockPool extends Iterable[StockContainer] with ToHTML{
  protected def newContainer(id : String, market : Market, baseMarket : Market) : StockContainer

  protected val containers = scala.collection.mutable.Map[Market, StockContainer]()

  def apply(id : String)(baseMarket : Market): StockContainer =
    containers.getOrElse(id, newContainer(id, id, baseMarket))

  def iterator: Iterator[StockContainer] =
    containers.iterator.map(_._2)

  def add(id : String, boughtMarket : Market, boughtAmount : Double, costBasis : Price, exchanger : Exchanger, date : LocalDateTime, exchangeRate : Price, exchangeMarket : Market, desc : String)(baseMarket : Market): Unit = {
    val container = containers.getOrElse(id, newContainer(id, boughtMarket, baseMarket))

    container.insert(
      Stock(boughtAmount, costBasis, exchanger, date, exchangeRate, exchangeMarket)
      , (x: Stock, y: Stock) => (x.costBasis - y.costBasis).abs < container.eps && x.exchanger == y.exchanger
          && x.date.sameDayAs(y.date)
          && x.exchangeMarket == y.exchangeMarket && (x.exchangeRate - y.exchangeRate).abs < container.eps
      , (x: Stock, y: Stock) => x.copy(amount = x.amount + y.amount)
      , desc
    )
    containers(id) = container
  }

  // Assumes id = boughtMarket. Useful for non-margin markets where ids are markets themselves
  def add(boughtMarket : Market, boughtAmount : Double, costBasis : Price, exchanger : Exchanger, date : LocalDateTime, exchangeRate : Price, exchangeMarket : Market, desc : String)(baseMarket : Market): Unit = {
    add(boughtMarket, boughtMarket, boughtAmount, costBasis, exchanger, date, exchangeRate, exchangeMarket, desc)(baseMarket)
  }

  def remove(id : String, soldAmount : Double)(baseMarket : Market)(date: LocalDateTime, exchanger: Exchanger, desc : String) : (Price, Price, StockContainer) = {
    val container = apply(id)(baseMarket)
    return container.removeAndGetBasis(soldAmount)(date: LocalDateTime, exchanger: Exchanger, desc)
  }

  override def toHTML: HTML =
    toHTML()

  def toHTML(caption : String = "") : HTML = {
    var totalCost = 0.0
    <table id='tableStyle1'>
      <tr>
        <th>Market</th>
        <th>Units</th>
        <th>Total cost</th>
        <th>Average cost</th>
        <th>Stock</th>
      </tr>
      {if(caption.nonEmpty)
        <caption>{caption}</caption>
      }
      {toList.sortBy(_.market).map{ stockContainer =>
        var amount = 0.0
        var cost = 0.0
        for (stock : Stock <- stockContainer) {
          amount += stock.amount
          cost += stock.amount * stock.costBasis
        }
        totalCost += cost
        if (cost > 0.001) {
          <tr>
            <td ><span class='market'>{stockContainer.market}</span></td>
            <td>{HTMLDoc.asMarket(amount, stockContainer.market, decimals = 4)}</td>
            <td>{HTMLDoc.asMarket(cost, stockContainer.baseMarket)}</td>
            <td class='noLineBreak'>{HTMLDoc.asMarket(cost / amount, stockContainer.baseMarket)} / <span class='market'>{stockContainer.market}</span></td>
            <td class='small2'>{stockContainer.toHTML(showTotal = false, showExchangeRates = true)}</td>
          </tr>
        }
       }
    }
    {val baseMarkets = this.map(_.baseMarket)
     val sameBaseMarkets = baseMarkets.nonEmpty && baseMarkets.tail.forall(_ == baseMarkets.head)
     if(sameBaseMarkets)
        <tr>
          <th>Total:</th>
          <th></th>
          <th>{HTMLDoc.asMarket(totalCost, baseMarkets.head)}</th>
          <th></th>
          <th></th>
        </tr>
    }
    </table>
  }
}


case class QueueStockPool() extends StockPool {
  def newContainer(id : String, market: Market, baseMarket : Market) =
    StockQueue(id, market, baseMarket)
}


case class StackStockPool() extends StockPool {
  def newContainer(id : String, market: Market, baseMarket : Market) =
    StockStack(id, market, baseMarket)
}