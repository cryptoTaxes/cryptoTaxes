package taxes

import java.text.SimpleDateFormat
import taxes.Exchanger.Exchanger
import taxes.Market.Market



// basis is expressed in base unit. exchanger is where it was bought
case class Stock(var amount : Double, costBasis : Price, exchanger : Exchanger, date : Date) {
  override def toString : String =
    "Stock(%.4f, %.8f, %s, ".format(amount, costBasis, exchanger) +
    new SimpleDateFormat("yyyy-MM-dd").format(date) +
    ")"
}


trait StockContainer extends Container[Stock] {
  val market : Market

  val baseMarket : Market

  def removeAndGetBasis(amount : Double) : (Price, Double) = {
    var toRemove = amount
    var basis = 0.0
    var done = false
    while(!this.isEmpty && !done) {
      val stock = this.first
      if(stock.amount >= toRemove) {
        basis += toRemove * stock.costBasis
        stock.amount -= toRemove
        done = true
      } else {
        basis += stock.amount * stock.costBasis
        toRemove -= stock.amount
        this.removeFirst()
      }
    }
    return if(done) (basis, 0) else (basis, toRemove)
  }

  def totalAmount : Double =
    this.iterator.map(_.amount).sum

  override def toString : String = {
    val amount = totalAmount
    val totalCost = this.iterator.map(p => p.amount * p.costBasis).sum

    val sb = StringBuilder.newBuilder
    sb.append(
      market.padTo(5, ' ')+
        Format.leftPad("%.6f".format(amount), 20, ' ')+
        Format.leftPad(Format.asMarket(totalCost, baseMarket), 20, ' ')+
        "  "
    )

    for(stock <- this)
      sb.append(
        "(%.6f, ".format(stock.amount)+
          Format.asMarket(stock.costBasis, baseMarket)+
          ", %s, ".format(stock.exchanger)+
          new SimpleDateFormat("yyyy-MM-dd").format(stock.date)+
          ") "
      )
    return sb.toString
  }
}


case class StockQueue(market : Market, baseMarket : Market) extends Queue[Stock] with StockContainer


case class StockStack(market : Market, baseMarket : Market) extends Stack[Stock] with StockContainer


trait StockPool {
  val baseMarket : Market

  protected def newContainer(market: Market) : StockContainer

  protected val containers = scala.collection.mutable.Map[Market, StockContainer]()

  def apply(market : Market): StockContainer =
    containers.getOrElse(market, newContainer(market))

  def iterator: Iterator[StockContainer] =
    containers.iterator.map(_._2)

  def foreach(f: (StockContainer ⇒ Unit)): Unit = {
    containers.map(_._2).foreach(f)
  }

  def add(boughtMarket : Market, boughtAmount : Double, costBasis : Price, exchanger : Exchanger, date : Date): Unit = {
    val container = apply(boughtMarket)

    container.addLast(
      Stock(boughtAmount, costBasis, exchanger, date)
      , (x: Stock, y: Stock) => x.costBasis == y.costBasis && x.exchanger == y.exchanger && x.date == y.date
      , (x: Stock, y: Stock) => x.copy(amount = x.amount + y.amount)
    )
    containers(boughtMarket) = container
  }

  def remove(soldMarket : Market, soldAmount : Double): (Price, Price) = {
    val container = apply(soldMarket)
    return container.removeAndGetBasis(soldAmount)
  }
}


case class QueueStockPool(baseMarket : Market) extends StockPool {
  def newContainer(market: Market) =
    StockQueue(market, baseMarket)
}


case class StackStockPool(baseMarket : Market) extends StockPool {
  def newContainer(market: Market) =
    StockStack(market, baseMarket)
}