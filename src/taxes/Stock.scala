package taxes

import java.text.SimpleDateFormat

import taxes.Exchanger.Exchanger
import taxes.Market.Market

import scala.xml.Elem



// basis is expressed in base unit. exchanger is where it was bought
case class Stock(var amount : Double, costBasis : Price, exchanger : Exchanger, date : Date) {
  override def toString : String =
    "Stock(%.4f, %.8f, %s, ".format(amount, costBasis, exchanger) +
    new SimpleDateFormat("yyyy-MM-dd").format(date) +
    ")"
}


trait StockContainer extends Container[Stock] with ToHTML {
  val id : String

  val market : Market  // what do we store

  val baseMarket : Market // what are prices expressed in

  def removeAndGetBasis(amount : Double) : (Price, Double, StockContainer) = {
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
        done = true
      } else {
        basis += stock.amount * stock.costBasis
        toRemove -= stock.amount
        this.removeFirst()
        usedStocks.insert(stock.copy())
      }
    }
    return if(done) (basis, 0, usedStocks) else (basis, toRemove, usedStocks)
  }

  def totalAmount : Double =
    this.iterator.map(_.amount).sum

  def totalCost : Double =
    this.iterator.map(p => p.amount * p.costBasis).sum

  override def toHTML : Elem = toHTML()

  def toHTML(showTotal : Boolean = true, showAmounts : Boolean = true) : Elem =
    <span>
      {if(showTotal) {
        <span>
          {Format.formatDecimal(totalAmount)}.
          {HTML.asMarket(totalCost, baseMarket)}.
        </span>
       }
      }
      {zipWithIndex.map{ case (stock, i) =>
      <span>
      <span class='noLineBreak'>
        <span>
          {Format.shortDf.format(stock.date)}
        </span>
        <span class='exchanger'>
          {stock.exchanger}
        </span>
        <span>
          {if(showAmounts)
            Format.formatDecimal(stock.amount)+" x"
           else
            ""
          }
          {HTML.asRate(stock.costBasis, baseMarket, market)}
            {if (i < iterator.length - 1) "," else ""}
        </span>
      </span>
      </span>
    }}
  </span>
}


case class StockQueue(id : String, market : Market, baseMarket : Market) extends Queue[Stock] with StockContainer


case class StockStack(id : String, market : Market, baseMarket : Market) extends Stack[Stock] with StockContainer


trait StockPool extends Iterable[StockContainer] with ToHTML{
  val baseMarket : Market

  protected def newContainer(id : String, market: Market) : StockContainer

  protected val containers = scala.collection.mutable.Map[Market, StockContainer]()

  def apply(id : String): StockContainer =
    containers.getOrElse(id, newContainer(id, "No market"))

  def iterator: Iterator[StockContainer] =
    containers.iterator.map(_._2)

  def add(id : String, boughtMarket : Market, boughtAmount : Double, costBasis : Price, exchanger : Exchanger, date : Date): Unit = {
    val container = containers.getOrElse(id, newContainer(id, boughtMarket))
    val eps =
      if(baseMarket==Market.euro || baseMarket==Market.usd)
        0.00001
      else
        0.00000001
    container.insert(
      Stock(boughtAmount, costBasis, exchanger, date)
      , (x: Stock, y: Stock) => (x.costBasis - y.costBasis).abs < eps && x.exchanger == y.exchanger && x.date.sameDayAs(y.date)
      , (x: Stock, y: Stock) => x.copy(amount = x.amount + y.amount)
    )
    containers(id) = container
  }

  // Assumes id = boughtMarket. Useful for non-margin markets where ids are markets themselves
  def add(boughtMarket : Market, boughtAmount : Double, costBasis : Price, exchanger : Exchanger, date : Date): Unit = {
    add(boughtMarket, boughtMarket, boughtAmount, costBasis, exchanger, date)
  }

  def remove(id : String, soldAmount : Double) : (Price, Price, StockContainer) = {
    val container = apply(id)
    return container.removeAndGetBasis(soldAmount)
  }

  override def toHTML: Elem =
    toHTML()

  def toHTML(caption : String = "") : Elem = {
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
      {toList.sortBy(_.market).map{ queue =>
        var amount = 0.0
        var cost = 0.0
        for (stock : Stock <- queue) {
          amount += stock.amount
          cost += stock.amount * stock.costBasis
        }
        totalCost += cost
        if (cost > 0.001) {
          <tr>
            <td ><span class='market'>{queue.market}</span></td>
            <td>{HTML.asMarket(amount, queue.market, decimals = 4)}</td>
            <td>{HTML.asMarket(cost, baseMarket)}</td>
            <td class='noLineBreak'>{HTML.asMarket(cost / amount, baseMarket)}/<span class='market'>{queue.market}</span></td>
            <td class='small2'>{queue.toHTML(showTotal = false)}</td>
          </tr>
        }
       }
    }
    <tr>
      <td class='embold'>Total:</td>
      <td></td>
      <td>{HTML.asMarket(totalCost, baseMarket)}</td>
      <td></td>
      <td></td>
    </tr>
    </table>
  }
}


case class QueueStockPool(baseMarket : Market) extends StockPool {
  def newContainer(id : String, market: Market) =
    StockQueue(id, market, baseMarket)
}


case class StackStockPool(baseMarket : Market) extends StockPool {
  def newContainer(id : String, market: Market) =
    StockStack(id, market, baseMarket)
}