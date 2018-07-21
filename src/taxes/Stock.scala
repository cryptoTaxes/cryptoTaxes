package taxes

import taxes.date._
import taxes.exchanger.Exchanger
import taxes.util.Logger
import spray.json._
import DefaultJsonProtocol._

object Stock {
  implicit val stockJson = jsonFormat6(Stock.apply)
}


// basis is expressed in base unit. exchanger is where it was bought
case class Stock(var amount : Double, costBasis : Price, exchanger : Exchanger, date : LocalDateTime, exchangeRate : Price, exchangeMarket : Market) {
  override def toString : String =
    f"Stock($amount%.4f, $costBasis%.8f, $exchanger, $exchangeRate%.8f, $exchangeMarket, ${Format.df.format(date)})"
}

object StockContainer {
  implicit object stockContainerJson extends RootJsonFormat[StockContainer] {
    val _containerType = "containerType"
    val _id = "id"
    val _market = "market"
    val _baseMarket = "baseMarket"
    val _finalBalance = "finalBalance"
    val _stocks = "stocks"

    val _stack = "stack"
    val _queue = "queue"

    def write(container: StockContainer) = {
      val containerType = container match {
        case _ : StockStack =>
          _stack
        case _ : StockQueue =>
          _queue
        case _ =>
          Logger.fatal("StockContainer.toFile: expecting a stack or a queue.")
      }
      JsObject(
        _containerType -> JsString(containerType),
        _id -> JsString(container.id),
        _market -> JsString(container.market),
        _baseMarket -> JsString(container.baseMarket),
        _finalBalance -> JsNumber(container.ledger.finalBalance),
        _stocks -> {
          val builder = new scala.collection.immutable.VectorBuilder[JsValue]()
          for(stock <- container.doubleEndedContainer)
            builder += stock.toJson
          JsArray(builder.result())
        }
      )
    }

    def read(value: JsValue) = {
      value.asJsObject.getFields(_containerType, _id, _market, _baseMarket, _finalBalance, _stocks) match {
        case Seq(JsString(containerType), JsString(id), JsString(market), JsString(baseMarket), JsNumber(finalBalance), JsArray(stocks)) =>
          var isStack = false
          val container : StockContainer = containerType match {
            case `_stack` =>
              isStack = true
              StockStack(id, market, baseMarket)
            case `_queue` =>
              StockQueue(id, market, baseMarket)
            case _ =>
              Logger.fatal(s"StockContainer.read: expecting a stack or a queue $containerType.")
          }
          container.ledger.initialBalance = finalBalance.doubleValue
          val vector = if(isStack) stocks.reverseIterator else stocks
          vector.foreach(x => container.insert(x.convertTo[Stock]))
          container
        case _ => throw new DeserializationException("StockContainer expected")
      }
    }
  }

  def fromFile(path : String) : StockContainer =
    FileSystem.withSource(path){ src =>
      JsonParser(src.mkString).convertTo[StockContainer]
  }
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

  val ledger = Ledger(market)

  def insert(x : Stock, desc : String): Unit = {
    ledger += Ledger.Entry(x.date, x.amount, x.exchanger, desc)
    insert(x)
  }

  def insert(x : Stock, eq : (Stock,Stock) => Boolean, combine : (Stock,Stock) => Stock, desc : String): Unit = {
    ledger += Ledger.Entry(x.date, x.amount, x.exchanger, desc)
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
    ledger += Ledger.Entry(date, -removed, exchanger, desc)
    return if(done) (basis, 0, usedStocks) else (basis, toRemove, usedStocks)
  }

  def totalAmount : Double =
    this.iterator.map(_.amount).sum

  def totalCost : Double =
    this.iterator.map(p => p.amount * p.costBasis).sum

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
              s"${Format.formatDecimal(stock.amount)} x"
             else
              ""
            }
            {HTMLDoc.asRate(stock.exchangeRate, stock.exchangeMarket, market)}
            =
          </span>
        }
        <span>
          {if(showAmounts)
            s"${Format.formatDecimal(stock.amount)} x"
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

  def toFile(path : String): Unit = {
    FileSystem.withPrintStream(path) {
      _.println(this.toJson.prettyPrint)
    }
    val (folder, name, ext) = FileSystem.decompose(path)
    val path2 = FileSystem.compose(Seq(folder,"ledger"), name, ext)
    FileSystem.withPrintStream(path2) {
      _.println(this.ledger.toJson.prettyPrint)
    }
  }
}

case class StockQueue(id : String, market : Market, baseMarket : Market) extends Queue[Stock] with StockContainer {
  override def copy: StockQueue = {
    val clone = StockQueue(id, market, baseMarket)
    for(x <- this)
      clone.insert(x.copy())
    return clone
  }
}


case class StockStack(id : String, market : Market, baseMarket : Market) extends Stack[Stock] with StockContainer {
  override def copy: StockStack = {
    val clone = StockStack(id, market, baseMarket)
    for(x <- this.reversed)
      clone.insert(x.copy())
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


  def loadFromDisk(path : String): Unit = {
    val src = new FolderSource[StockContainer](path, FileSystem.stockExtension) {
      override def fileSource(fileName: String): FileSource[StockContainer] =
        new FileSource[StockContainer](fileName) {
          override def read(): Seq[StockContainer] =
            Seq(StockContainer.fromFile(fileName))
        }
    }

    for(stockContainer <- src.read())
      if(stockContainer.ledger.initialBalance > 1.0E-7 || stockContainer.nonEmpty) {
        containers(stockContainer.id) = stockContainer
      }
  }

  def saveToDisk(path : String): Unit = {
    for(stockContainer <- containers.values) {
      val filePath = FileSystem.compose(Seq(path), stockContainer.id, FileSystem.stockExtension)
      stockContainer.toFile(filePath)
    }
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