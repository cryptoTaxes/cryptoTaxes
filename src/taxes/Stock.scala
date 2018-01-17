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


case class StockQueue(market : Market, baseMarket : Market) extends DEQueue[Stock] {
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
}


case class StockQueuePool(baseMarket : Market) {
  private val queues = scala.collection.mutable.Map[Market, StockQueue]()

  def apply(market : Market): StockQueue =
    queues.getOrElse(market, StockQueue(market, baseMarket))

  def iterator: Iterator[StockQueue] =
    queues.iterator.map(_._2)

  def foreach(f: ((Market, StockQueue)) ⇒ Unit): Unit = {
    queues.foreach(f)
  }

  def add(boughtMarket : Market, boughtAmount : Double, costBasis : Price, exchanger : Exchanger, date : Date): Unit = {
    val qBought = apply(boughtMarket)

    qBought.addLast(
        Stock(boughtAmount, costBasis, exchanger, date)
      , (x: Stock, y: Stock) => x.costBasis == y.costBasis && x.exchanger == y.exchanger && x.date == y.date
      , (x: Stock, y: Stock) => Stock(x.amount + y.amount, x.costBasis, x.exchanger, x.date)
      )
    queues(boughtMarket) = qBought
  }

  def remove(soldMarket : Market, soldAmount : Double): (Price, Price) = {
    val qSold = apply(soldMarket)
    return qSold.removeAndGetBasis(soldAmount)
  }
}