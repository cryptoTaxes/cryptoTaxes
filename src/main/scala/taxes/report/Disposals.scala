package taxes.report

import taxes.collection.Grouped
import taxes.{Config, Currency, Format, HTML, HTMLDoc, Price, Processed, RichText, report}
import taxes.date.LocalDateTime
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
          val disposal = Disposal(
              exchange.exchange.date
            , stock.amount
            , exchange.exchange.exchanger
            , exchange.soldPriceInBaseCurrency
            , RichText(s"Exchange ${RichText.report(exchange.exchange.date.getYear, exchange.operationNumber, showYear = true)}")
            , stock.date
            , stock.exchanger
            , stock.costBasisPrice
            , RichText(RichText.report(stock.date.getYear, stock.operationNumber, showYear = true))
            , stock.amount * (exchange.soldPriceInBaseCurrency - stock.costBasisPrice)
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
            , -(stock.amount * stock.costBasisPrice)
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
            , -(stock.amount * stock.costBasisPrice)
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

  def printToCSVFile(path: String): Unit = {
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
            <td class='alignR small1'>
              {Format.df.format(disposal.date)}
            </td>
            <td class='alignR darkRed'>
              {Format.formatDecimal(-disposal.amount, report.decimalPlaces(currency))}
            </td>
            <td class='exchanger'>
              {disposal.exchanger}
            </td>
            <td class='alignR'>
              {HTMLDoc.asRate(disposal.priceDisposed, baseCurrency, currency, decimals = report.decimalPlaces(baseCurrency), small = true)}
            </td>
            <td class='small1 paddingR'>
              {disposal.reference.toHTML}
            </td>

            <td class='paddingL small1 barL'>
              {Format.df.format(disposal.dateAcquired)}
            </td>
            <td class='exchanger'>
              {disposal.exchangerAcquired}
            </td>
            <td class='alignR'>
              {HTMLDoc.asRate(disposal.costBasisPrice, baseCurrency, currency, decimals = report.decimalPlaces(baseCurrency), small = true)}
            </td>
            <td class='small1 alignR paddingR'>
              {disposal.referenceAcquired.toHTML}
            </td>
            <td class={"alignR barL "+(if(disposal.gainLoss>=0) "darkBlue" else "darkRed")}>
              {HTMLDoc.asCurrency(disposal.gainLoss, baseCurrency, report.decimalPlaces(baseCurrency), small = true)}
            </td>
          </tr>
        }
        }}{if (numEntries > 0)
        <tr>
          <th></th>
          <th>Total disposed:</th>
          <th class='alignR'>
            {Format.formatDecimal(report.showBalance(totalDisposedAmount), report.decimalPlaces(currency))}
          </th>
          <th>Average:</th>
          <th class='alignR'>
            {HTMLDoc.asRate(totalProceeds / totalDisposedAmount, baseCurrency, currency, report.decimalPlaces(baseCurrency), small = true)}
          </th>
          <th></th>
          <th class='barL'></th>
          <th>Average:</th>
          <th class='alignR'>
            {HTMLDoc.asRate(totalCostBasis / totalDisposedAmount, baseCurrency, currency, decimals = report.decimalPlaces(baseCurrency), small = true)}
          </th>
          <th></th>
          <th class={"barL alignR "+(if(totalGainLoss>=0) "darkBlue" else "darkRed")}>
            {HTMLDoc.asCurrency(totalGainLoss, baseCurrency, report.decimalPlaces(baseCurrency), small = true)}
          </th>
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
