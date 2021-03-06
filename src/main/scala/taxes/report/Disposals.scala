package taxes.report

import taxes.collection.Grouped
import taxes.{Config, Currency, Format, HTML, HTMLDoc, Price, Processed, RichText, report}
import taxes.date._
import taxes.exchanger.Exchanger
import taxes.io.FileSystem

object Disposals {
  private case class
    Disposal(date: LocalDateTime
             , amount: Double
             , exchanger: Exchanger
             , priceDisposed: Price
             , reference: RichText
             , dateAcquired: LocalDateTime
             , exchangerAcquired: Exchanger
             , costBasisPrice: Price
             , referenceAcquired: RichText
             , gainLoss: Double
             )
}

case class Disposals(baseCurrency: Currency, processedOperations: Seq[Processed], year: Int) {
  import Disposals._

  private val grouped = Grouped[Currency, Disposal]()

  preprocess()

  private def preprocess(): Unit =
    processedOperations.foreach(process)

  private def process(processed: Processed): Unit ={
    processed match {
      case exchange: Processed.Exchange =>
        for (stock <- exchange.disposedStocks) {
          val currency = exchange.exchange.fromCurrency
          val soldPriceInBaseCurrency = if(stock.amount==0 || exchange.soldPriceInBaseCurrency.isNaN) 0.0 else exchange.soldPriceInBaseCurrency
          val disposal = Disposal(
              exchange.exchange.date
            , stock.amount
            , exchange.exchange.exchanger
            , soldPriceInBaseCurrency
            , RichText(s"Exchange ${RichText.report(exchange.exchange.date.getYear, exchange.operationNumber, showYear = true)}")
            , stock.date
            , stock.exchanger
            , stock.costBasisPrice
            , RichText(RichText.report(stock.date.getYear, stock.operationNumber, showYear = true))
            , stock.amount * (soldPriceInBaseCurrency - stock.costBasisPrice)
            )
          grouped.append(currency, disposal)
        }
        /*
        if(Config.config.deprecatedUp2017Version) {
          val feeDisposal = Disposal(
            exchange.exchange.date
            , exchange._deprecated_feeAmount
            , exchange.exchange.exchanger
            , exchange._deprecated_feeInBaseCurrency / exchange._deprecated_feeAmount
            , RichText(s"Fee ${RichText.report(exchange.exchange.date.getYear, exchange.operationNumber, showYear = true)}")
            , exchange.exchange.date
            , exchange.exchange.exchanger
            , exchange._deprecated_feeInBaseCurrency / exchange._deprecated_feeAmount
            , RichText(s"Fee ${RichText.report(exchange.exchange.date.getYear, exchange.operationNumber, showYear = true)}")
            , exchange._deprecated_feeInBaseCurrency
          )
          grouped.append(exchange._deprecated_feeCurrency, feeDisposal)
        }*/

      case loss: Processed.Loss =>
        for (stock <- loss.disposedStocks) {
          val currency = loss.loss.currency
          val disposal = Disposal(
              loss.loss.date
            , stock.amount
            , loss.loss.exchanger
            , stock.costBasisPrice
            , RichText(s"Loss ${RichText.report(loss.loss.date.getYear, loss.operationNumber, showYear = true)}")
            , stock.date
            , stock.exchanger
            , stock.costBasisPrice
            , RichText(RichText.report(stock.date.getYear, stock.operationNumber, showYear = true))
            , if(currency==baseCurrency) 0 else -(stock.amount * stock.costBasisPrice)
          )
          grouped.append(currency, disposal)
        }

      case fee: Processed.Fee =>
        for (stock <- fee.disposedStocks) {
          val currency = fee.fee.currency
          val disposal = Disposal(
              fee.fee.date
            , stock.amount
            , fee.fee.exchanger
            , stock.costBasisPrice
            , RichText(s"Fee ${RichText.report(fee.fee.date.getYear, fee.operationNumber, showYear = true)}")
            , stock.date
            , stock.exchanger
            , stock.costBasisPrice
            , RichText(RichText.report(stock.date.getYear, stock.operationNumber, showYear = true))
            , if(currency==baseCurrency) 0 else -(stock.amount * stock.costBasisPrice)
          )
          grouped.append(currency, disposal)
        }

      case composed: Processed.Composed =>
        composed.processed.foreach(process)
      case _ =>
        ;
    }
  }

  private val title =
    s"Disposed Stocks $year"

  private def defaultFile(ext: String): String =
    s"${FileSystem.userOutputFolder(year)}/DisposedStocks.$year.$ext"

  def printToCSVFile(): Unit =
    printToCSVFile(defaultFile("csv"))

  def printToCSVFile(path: String): Unit =
    FileSystem.withPrintStream(path) { ps =>
      val sep = ", "
      ps.println(s"$sep$title")
      ps.println()
      val header = Seq("", "Date Disposed","Amount","Exchanger","Proceeds","","Reference",
        "Date Acquired","Exchanger","Cost Basis","", "Reference",
        "Gain/Loss", "", "Holding", "")
      for((currency, disposals) <- grouped) {
        ps.println(s"$currency$sep${Currency.nameOf(currency).getOrElse("")}")
        ps.println(header.mkString(sep))
        var i = 0
        disposals.foreach{ disposal =>
          i += 1
          val line = Seq( i
            , disposal.date.format(taxes.Format.df)
            , -disposal.amount
            , disposal.exchanger
            , disposal.priceDisposed
            , s"$baseCurrency / $currency"
            , disposal.reference.toString
            , disposal.dateAcquired.format(taxes.Format.df)
            , disposal.exchangerAcquired
            , disposal.costBasisPrice
            , s"$baseCurrency / $currency"
            , disposal.referenceAcquired.toString
            , disposal.gainLoss
            , baseCurrency
            , taxes.Format.asTimeDiff(disposal.date.difference(disposal.dateAcquired))
            , if(disposal.date.atLeast1YearFrom(disposal.dateAcquired)) "*" else ""
          )
          ps.println(line.mkString(sep))
        }
        ps.println()
        ps.println()
      }
    }

  def printToHTMLFile(): Unit =
    printToHTMLFile(defaultFile("html"))

  def printToHTMLFile(path: String): Unit = {
    val htmlDoc = HTMLDoc(path, title)

    htmlDoc += <div class='header'>{title}</div>
    for(currency <- grouped.map(_._1))
      toHTML(currency) match {
        case None => ;
        case Some(html) => htmlDoc += html
      }
    htmlDoc.close()
  }

  private def toHTML(currency: Currency): Option[HTML] = {
    var numEntries = 0
    var totalDisposedAmount = 0.0
    var totalProceeds = 0.0
    var totalCostBasis = 0.0
    var totalGainLoss = 0.0
    val table =
      <table id='tableStyle1'>
        <tr>
          <th></th>
          <th>Date Disposed</th>
          <th class='alignR'>Amount</th>
          <th class='alignR'>Exchanger</th>
          <th class='alignR'>Proceeds</th>
          <th class='alignL'>Reference</th>
          <th class='barL'>Date Acquired</th>
          <th class='alignR'>Exchanger</th>
          <th class='alignR'>Cost Basis</th>
          <th class='alignL'>Reference</th>
          <th class='alignR barL'>Gain/Loss</th>
          <th class='alignR'>Holding</th>
        </tr>
        <caption>
          {Currency.fullName(currency)}
        </caption>{
        grouped(currency).map { disposal => {
          numEntries += 1
          totalDisposedAmount += disposal.amount
          val proceeds = disposal.amount * disposal.priceDisposed
          totalProceeds += proceeds
          val costBasis = disposal.amount * disposal.costBasisPrice
          totalCostBasis += costBasis
          totalGainLoss += disposal.gainLoss
          <tr>
            <td class='alignR small1'>
              {numEntries}
            </td>
            <td>
              {report.Format.asDate(disposal.date)}
            </td>
            <td class='alignR'>
              {report.Format.asAmount(-disposal.amount, currency)}
            </td>
            <td class='exchanger'>
              {disposal.exchanger}
            </td>
            <td class='alignR'>
              {report.Format.asRate(disposal.priceDisposed, baseCurrency, currency)}
            </td>
            <td class='small1 paddingR'>
              {disposal.reference.toHTML}
            </td>

            <td class='barL'>
              {report.Format.asDate(disposal.dateAcquired)}
            </td>
            <td class='exchanger'>
              {disposal.exchangerAcquired}
            </td>
            <td class='alignR'>
              {report.Format.asRate(disposal.costBasisPrice, baseCurrency, currency)}
            </td>
            <td class='small1 alignR paddingR'>
              {disposal.referenceAcquired.toHTML}
            </td>
            <td class='alignR barL'>
              {report.Format.asCurrency(disposal.gainLoss, baseCurrency)}
            </td>
            <td class='alignR small1'>
              {report.Format.asTimeDiff(disposal.date, disposal.dateAcquired)}
            </td>
          </tr>
        }
        }}{if (numEntries > 0)
        <tr>
          <th></th>
          <th>Total disposed:</th>
          <th class='alignR'>
            {report.Format.asAmount(-totalDisposedAmount, currency)}
          </th>
          <th>Average:</th>
          <th class='alignR'>
            {report.Format.asRate(totalProceeds / totalDisposedAmount, baseCurrency, currency)}
          </th>
          <th></th>
          <th class='barL'></th>
          <th>Average:</th>
          <th class='alignR'>
            {report.Format.asRate(totalCostBasis / totalDisposedAmount, baseCurrency, currency)}
          </th>
          <th></th>
          <th class='barL alignR'>
            {report.Format.asCurrency(totalGainLoss, baseCurrency)}
          </th>
          <th></th>
        </tr>}
      </table>
    if (numEntries > 0)
      Some {
        <span id={currency}>
          {table}
        </span>
      }
    else
      None
  }
}
