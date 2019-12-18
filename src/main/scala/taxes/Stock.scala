package taxes

import taxes.date._
import taxes.exchanger.Exchanger
import taxes.util.Logger
import spray.json._
import spray.json.JsonProtocol._
import taxes.io.FileSystem

object Stock {
  implicit val stockJson = jsonFormat6(Stock.apply)
}


// basis is expressed in base unit. exchanger is where it was bought
case class Stock(var amount: Double, costBasis: Price, exchanger: Exchanger, date: LocalDateTime, exchangeRate: Price, exchangeCurrency: Currency) {
  override def toString: String =
    f"Stock($amount%.4f, $costBasis%.8f, $exchanger, $exchangeRate%.8f, $exchangeCurrency, ${Format.df.format(date)})"
}

object StockContainer {
  implicit object stockContainerJson extends RootJsonFormat[StockContainer] {
    val _containerType = "containerType"
    val _id = "id"
    val _currency = "currency"
    val _baseCurrency = "baseCurrency"
    val _stocks = "stocks"
    val _ledger = "ledger"

    val _stack = "stack"
    val _queue = "queue"

    def write(container: StockContainer) = {
      val containerType = container match {
        case _: StockStack =>
          _stack
        case _: StockQueue =>
          _queue
        case _ =>
          Logger.fatal("stockContainerJson: expecting a stack or a queue.")
      }
      JsObject(
        _containerType -> JsString(containerType),
        _id -> JsString(container.id),
        _currency -> JsString(container.currency),
        _baseCurrency -> JsString(container.baseCurrency),
        _stocks -> {
          val builder = new scala.collection.immutable.VectorBuilder[JsValue]()
          for(stock <- container.doubleEndedContainer)
            builder += stock.toJson
          JsArray(builder.result())
        },
        _ledger -> container.ledger.toJson
      )
    }

    def read(value: JsValue) =
      try {
        value.asJsObject.getFields(_containerType, _id, _currency, _baseCurrency, _stocks, _ledger) match {
          case Seq(JsString(containerType), JsString(id), JsString(currency), JsString(baseCurrency), JsArray(stocks), jsLedger) =>
            val ledger: Ledger = jsLedger.convertTo[Ledger]
            var isStack = false
            val container: StockContainer = containerType match {
              case `_stack` =>
                isStack = true
                StockStack(id, currency, baseCurrency, ledger)
              case `_queue` =>
                StockQueue(id, currency, baseCurrency, ledger)
              case _ =>
                Logger.fatal(s"StockContainer.read: expecting a stack or a queue $containerType.")
            }
            val vector = if(isStack) stocks.reverseIterator else stocks
            vector.foreach(x => container.insert(x.convertTo[Stock]))
            container
        }
      } catch {
        case _ => throw DeserializationException(s"StockContainer expected in $value")
      }
  }

  def fromFile(path: String): StockContainer =
    FileSystem.withSource(path){ src =>
      src.mkString.parseJson.convertTo[StockContainer]
  }
}

sealed trait StockContainer extends Container[Stock] with ToHTML {
  val id: String

  val currency: Currency  // what do we store

  val baseCurrency: Currency // what are prices expressed in

  val ledger: Ledger

  def copy: StockContainer

  val eps: Double =
    if(Config.config.deprecatedUp2017Version) {
      // this is really a bug, fixed below for non-deprecated version
      if(baseCurrency == Currency.euro || baseCurrency == Currency.usd)
        0.00001
      else
        Config.config.epsilon
    } else {
      if(currency == Currency.euro || currency == Currency.usd)
        0.00001
      else
        Config.config.epsilon
    }

  def insert(stock: Stock, description: String): Unit = {
    ledger += Ledger.Entry(stock.date, stock.amount, stock.exchanger, description)
    insert(stock)
  }

  def insert(stock: Stock, eq: (Stock,Stock) => Boolean, combine: (Stock,Stock) => Stock, description: String): Unit = {
    ledger += Ledger.Entry(stock.date, stock.amount, stock.exchanger, description)
    insert(stock, eq, combine)
  }

  def removeAndGetBasis(amount: Double)(date: LocalDateTime, exchanger: Exchanger, description: String): (Price, Double, StockContainer) = {
    var toRemove = amount
    var basis = 0.0
    var done = false
    val usedStocks = new StockQueue(id, currency, baseCurrency)
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
    ledger += Ledger.Entry(date, -removed, exchanger, description)
    return if(done) (basis, 0, usedStocks) else (basis, toRemove, usedStocks)
  }

  def totalAmount: Double =
    this.iterator.map(_.amount).sum

  def totalCost: Double =
    this.iterator.map(p => p.amount * p.costBasis).sum

  override def toHTML: HTML = toHTML()

  def toHTML(showTotal: Boolean = true, showAmounts: Boolean = true, showExchangeRates: Boolean = false): HTML =
    <span>
      {if(showTotal) {
        <span>
          {HTMLDoc.asCurrency(totalAmount, currency)}.
          {HTMLDoc.asCurrency(totalCost, baseCurrency)}.
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
        {if(showExchangeRates && stock.exchangeCurrency != baseCurrency)
          <span>
            {if(showAmounts)
              s"${Format.formatDecimal(stock.amount)} x"
             else
              ""
            }
            {HTMLDoc.asRate(stock.exchangeRate, stock.exchangeCurrency, currency)}
            =
          </span>
        }
        <span>
          {if(showAmounts)
            s"${Format.formatDecimal(stock.amount)} x"
           else
            ""
          }
          {HTMLDoc.asRate(stock.costBasis, baseCurrency, currency)}
            {if(i < size - 1) "," else ""}
        </span>
      </span>
      </span>
    }}
  </span>

  def saveToFile(path: String): Unit = {
    FileSystem.withPrintStream(path) { ps =>
      ps.println(this.toJson.prettyPrint)
    }
    val (folder, name, ext) = FileSystem.decompose(path)
    val path2 = FileSystem.compose(Seq(folder,"ledger"), name, ext)
    FileSystem.withPrintStream(path2) { ps =>
      ps.println(this.ledger.toJson.prettyPrint)
    }
  }
}

case class StockQueue(id: String, currency: Currency, baseCurrency: Currency, ledger: Ledger) extends Queue[Stock] with StockContainer {
  def this(id: String, currency: Currency, baseCurrency: Currency) {
    this(id, currency, baseCurrency, Ledger(id, 0))
  }

  override def copy: StockQueue = {
    val clone = StockQueue(id, currency, baseCurrency, ledger.copy)
    for(x <- this)
      clone.insert(x.copy())
    return clone
  }
}


case class StockStack(id: String, currency: Currency, baseCurrency: Currency, ledger: Ledger) extends Stack[Stock] with StockContainer {
  def this(id: String, currency: Currency, baseCurrency: Currency) {
    this(id, currency, baseCurrency, Ledger(id, 0))
  }

  override def copy: StockStack = {
    val clone = StockStack(id, currency, baseCurrency, ledger.copy)
    for(x <- this.reversed)
      clone.insert(x.copy())
    return clone
  }
}


trait StockPool extends Iterable[StockContainer] with ToHTML{
  protected def newContainer(id: String, currency: Currency, baseCurrency: Currency): StockContainer

  protected val containers = scala.collection.mutable.Map[Currency, StockContainer]()

  def delete(id: String): Unit = {
    containers -= id
  }

  def apply(id: String)(baseCurrency: Currency): StockContainer =
    containers.getOrElse(id, newContainer(id, id, baseCurrency))

  def iterator: Iterator[StockContainer] =
    containers.iterator.map(_._2)

  def add(id: String, boughtCurrency: Currency, boughtAmount: Double, costBasis: Price, exchanger: Exchanger, date: LocalDateTime, exchangeRate: Price, exchangeCurrency: Currency, desc: String)(baseCurrency: Currency): Unit = {
    val container = containers.getOrElse(id, newContainer(id, boughtCurrency, baseCurrency))

    container.insert(
      Stock(boughtAmount, costBasis, exchanger, date, exchangeRate, exchangeCurrency)
      , (x: Stock, y: Stock) => (x.costBasis - y.costBasis).abs < container.eps && x.exchanger == y.exchanger
          && x.date.sameDayAs(y.date)
          && x.exchangeCurrency == y.exchangeCurrency && (x.exchangeRate - y.exchangeRate).abs < container.eps
      , (x: Stock, y: Stock) => x.copy(amount = x.amount + y.amount)
      , desc
    )
    containers(id) = container
  }

  // Assumes id = boughtCurrency. Useful for non-margin currencies where ids are currencies themselves
  def add(boughtCurrency: Currency, boughtAmount: Double, costBasis: Price, exchanger: Exchanger, date: LocalDateTime, exchangeRate: Price, exchangeCurrency: Currency, desc: String)(baseCurrency: Currency): Unit = {
    add(boughtCurrency, boughtCurrency, boughtAmount, costBasis, exchanger, date, exchangeRate, exchangeCurrency, desc)(baseCurrency)
  }

  def remove(id: String, soldAmount: Double)(baseCurrency: Currency)(date: LocalDateTime, exchanger: Exchanger, desc: String): (Price, Price, StockContainer) = {
    val container = apply(id)(baseCurrency)
    return container.removeAndGetBasis(soldAmount)(date: LocalDateTime, exchanger: Exchanger, desc)
  }

  override def toHTML: HTML =
    toHTML()

  def toHTML(caption: String = ""): HTML = {
    var totalCost = 0.0
    <table id='tableStyle1'>
      <tr>
        <th>Currency</th>
        <th>Units</th>
        <th>Total cost</th>
        <th>Average cost</th>
        <th>Stock</th>
      </tr>
      {if(caption.nonEmpty)
        <caption>{caption}</caption>
      }
      {toList.sortBy(_.currency).map{ stockContainer =>
        var amount = 0.0
        var cost = 0.0
        for(stock: Stock <- stockContainer) {
          amount += stock.amount
          cost += stock.amount * stock.costBasis
        }
        totalCost += cost
        if(totalCost > 0) {
          <tr>
            <td ><span class='currency'>{stockContainer.currency}</span></td>
            <td>{HTMLDoc.asCurrency(amount, stockContainer.currency, decimals = 4.max(Config.config.decimalPlaces))}</td>
            <td>{HTMLDoc.asCurrency(cost, stockContainer.baseCurrency)}</td>
            <td class='noLineBreak'>{HTMLDoc.asCurrency(cost / amount, stockContainer.baseCurrency)} / <span class='currency'>{stockContainer.currency}</span></td>
            <td class='small2'>{stockContainer.toHTML(showTotal = false, showExchangeRates = true)}</td>
          </tr>
        }
       }
    }
    {val baseCurrencies = this.map(_.baseCurrency)
     val sameBaseCurrencies = baseCurrencies.nonEmpty && baseCurrencies.tail.forall(_ == baseCurrencies.head)
     if(sameBaseCurrencies)
        <tr>
          <th>Total:</th>
          <th></th>
          <th>{HTMLDoc.asCurrency(totalCost, baseCurrencies.head)}</th>
          <th></th>
          <th></th>
        </tr>
    }
    </table>
  }

  def printToCSVFile(csvFileName: String, year: Int): Unit = {
    FileSystem.withPrintStream(csvFileName) { ps =>
      ps.println()
      ps.println(s"$year Final Portfolio")
      ps.println("")

      val sep = ";"
      val decimals = 8

      toList.filter(_.nonEmpty).sortBy(_.currency).foreach { stockContainer =>
        val baseCurrency = stockContainer.baseCurrency
        val header = List("date", "exchanger", "amount", s"cost basis ($baseCurrency)", s"subtotal ($baseCurrency)", "total amount", s"total cost ($baseCurrency)", s"average cost ($baseCurrency)")
        ps.println(stockContainer.currency)
        ps.println(header.mkString(sep))

        var amount = 0.0
        var cost = 0.0
        for(stock: Stock <- stockContainer) {
          amount += stock.amount
          val subtotal = stock.amount * stock.costBasis
          cost += subtotal
          val line = List[Any](stock.date.format(Format.df), stock.exchanger
            , Format.formatDecimal(stock.amount, decimals)
            , Format.formatDecimal(stock.costBasis, decimals)
            , Format.formatDecimal(subtotal, decimals)
            , Format.formatDecimal(amount, decimals)
            , Format.formatDecimal(cost, decimals)
            , Format.formatDecimal(cost / amount, decimals)
          )
          ps.println(line.mkString(sep))
        }
        ps.println()
        ps.println()
        ps.println()
      }
    }
  }

  def loadFromDisk(path: String): Unit = {
    val src = new FolderSource[StockContainer](path, FileSystem.stockExtension) {
      override def fileSource(fileName: String): FileSource[StockContainer] =
        new FileSource[StockContainer](fileName) {
          override def read(): Seq[StockContainer] =
            Seq(StockContainer.fromFile(fileName))
        }
    }

    def relevant(stockContainer: StockContainer) =
      stockContainer.nonEmpty

    for(stockContainer <- src.read())
      if(relevant(stockContainer)) {
        containers(stockContainer.id) = stockContainer
      }
  }

  def saveToDisk(path: String): Unit = {
    for(stockContainer <- containers.values) {
      val filePath = FileSystem.compose(Seq(path), stockContainer.id, FileSystem.stockExtension)
      stockContainer.saveToFile(filePath)
    }
  }
}


case class QueueStockPool() extends StockPool {
  def newContainer(id: String, currency: Currency, baseCurrency: Currency) =
    new StockQueue(id, currency, baseCurrency)
}


case class StackStockPool() extends StockPool {
  def newContainer(id: String, currency: Currency, baseCurrency: Currency) =
    new StockStack(id, currency, baseCurrency)
}