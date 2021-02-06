package taxes

import spray.json._
import spray.json.JsonProtocol._
import taxes.collection.Queue
import StockContainer.stockContainerJson.{_baseCurrency, _currency}
import taxes.date._
import taxes.exchanger.Exchanger
import taxes.io.FileSystem
import taxes.report.Format.asRate

object DisposedStock {
  implicit val disposedStockJson = jsonFormat9(DisposedStock.apply)
}

case class DisposedStock(date: LocalDateTime, amount: Price, exchanger: Exchanger, description: RichText, acquiredDate: LocalDateTime, remaining: Price, acquiredExchanger: Exchanger, costBasis: Price, acquiredOperationNumber: Int)

object DisposedStocksQueue {
  val decimalPlaces = 4.max(Config.config.decimalPlaces)

  implicit val disposedStocksQueueJson = new RootJsonFormat[DisposedStocksQueue] {
    val _containerType = "containerType"
    val _elems = "elems"

    val _queue = "queue"

    def write(queue: DisposedStocksQueue) = {
      val containerType = _queue
      JsObject(
        _containerType -> JsString(containerType),
        _currency -> JsString(queue.currency),
        _baseCurrency -> JsString(queue.baseCurrency),
        _elems -> {
          val builder = new scala.collection.immutable.VectorBuilder[JsValue]()
          for (elem <- queue)
            builder += elem.toJson
          JsArray(builder.result())
        }
      )
    }

    override def read(value: JsValue): DisposedStocksQueue =
      try {
        value.asJsObject.getFields(_containerType, _currency, _baseCurrency, _elems) match {
          case Seq(JsString(containerType), JsString(currency), JsString(baseCurrency), JsArray(elems)) =>
            var isStack = false
            val container = new DisposedStocksQueue(currency, baseCurrency)
            elems.foreach(x => container.insert(x.convertTo[DisposedStock]))
            container
        }
      } catch {
        case _ => throw DeserializationException(s"DisposedStocksQueue.disposedStocksQueueJson.read: Container expected in $value")
      }
  }
}

class DisposedStocksQueue(
    val currency: Currency  // what do we store
  , val baseCurrency: Currency // what are prices expressed in
  ) extends Queue[DisposedStock] {

  def toHTML: Option[HTML] = {
    var numEntries = 0
    var totalDisposedAmount = 0.0
    val table =
      <table id='tableStyle1'>
        <tr>
          <th></th>
          <th>Date Disposed</th>
          <th>Amount</th>
          <th class='alignL paddingL'>Exchanger</th>
          <th class='alignL'>Description</th>
          <th class='barL'>Date Acquired</th>
          <th>Hold</th>
          <th>Remaining</th>
          <th class='alignL paddingL'>Exchanger</th>
          <th class='alignR'>Cost Basis</th>
          <th class='alignL'>Description</th>
        </tr>
        <caption>{Currency.fullName(currency)}</caption>

        {map{ stock => {
          numEntries += 1
          totalDisposedAmount += stock.amount
          <tr>
            <td class='alignR'>{numEntries}</td>
            <td class='paddingL'>{Format.df.format(stock.date)}</td>
            <td class='paddingL darkRed'>
              {Format.formatDecimal(-stock.amount, DisposedStocksQueue.decimalPlaces)}
            </td>
            <td class='exchanger alignL paddingL'>{stock.exchanger}</td>
            <td class='alignL'>{stock.description.toHTML}</td>
            <td class='paddingL barL'>{Format.df.format(stock.acquiredDate)}</td>
            <td class='alignR'>{
              val diff = stock.date.difference(stock.acquiredDate)
              s"$diff.years years, $diff.days days"}
            </td>
            <td class='paddingL darkBlue'>
              {if(stock.remaining>0) Format.formatDecimal(stock.remaining, DisposedStocksQueue.decimalPlaces)}
            </td>
            <td class='exchanger alignL paddingL'>{stock.acquiredExchanger}</td>
            <td>{asRate(stock.costBasis, baseCurrency, currency)}</td>
            <td>
              {RichText(RichText.report(stock.acquiredDate.getYear, stock.acquiredOperationNumber, showYear = true)).toHTML}
            </td>
          </tr>
      }}
      }
      {if(numEntries>0)
        <tr>
          <th></th>
          <th>Total disposed:</th>
          <th>{Format.formatDecimal(Ledger.showBalance(totalDisposedAmount), DisposedStocksQueue.decimalPlaces)}</th>
          <th></th>
          <th></th>
          <th></th>
          <th></th>
          <th></th>
          <th></th>
          <th></th>
          <th></th>
        </tr>
      }
      </table>
    if(numEntries>0)
      Some{<span id={currency}>{table}</span>}
    else
      None
  }

  def printToCSV(ps: FileSystem.PrintStream): Unit = {
    if(this.nonEmpty) {
      ps.println(Currency.fullName(currency))

      val sep = ";"
      val header = List("", "Date disposed", "Amount", "Exchanger", "Description", "Date acquired", "Remaining", "Exchanger", "Cost basis", "", "Description")

      var numEntries = 0
      ps.println(header.mkString(sep))
      for(stock <- this) {
        numEntries += 1
        ps.println(List[Any](numEntries, stock.date.format(Format.shortDf), stock.amount, stock.exchanger, stock.description.toString
          , stock.acquiredDate.format(Format.shortDf), stock.remaining, stock.acquiredExchanger
          , stock.costBasis, s"$baseCurrency / $currency"
          , RichText(RichText.report(stock.acquiredDate.getYear, stock.acquiredOperationNumber, showYear = true)).toString).mkString(sep))
      }
      ps.println()
      ps.println()
    }
  }
}
