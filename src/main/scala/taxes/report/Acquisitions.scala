package taxes.report

import taxes.collection.Grouped
import taxes.{Config, Currency, Format, HTML, HTMLDoc, Processed, RichText, report}
import taxes.date.LocalDateTime
import taxes.exchanger.Exchanger
import taxes.io.FileSystem

object Acquisitions {
  private case class Acquisition(
      date: LocalDateTime
    , amount: Double
    , costBasis: Double
    , exchanger: Exchanger
    , exchangeRate:Double
    , exchangeCurrency: Currency
    , operationNumber: Int
    )
}

case class Acquisitions(baseCurrency: Currency, processedOperations: Seq[Processed]) {
  import Acquisitions._

  def preprocess(): Unit =
    processedOperations.foreach(process)

  private val grouped = Grouped[Currency, Acquisition]()

  private def process(processed: Processed): Unit ={
    processed match {
      case exchange: Processed.Exchange =>
        val currency = exchange.exchange.toCurrency
        var amount = exchange.exchange.toAmount

        if(!Config.config.deprecatedUp2017Version)
          for(pair <- exchange.exchange.fees)
            if(pair.currency == currency)
              amount += pair.amount
        if(amount>0) {
          val acquisition = Acquisition(exchange.exchange.date, amount, exchange.boughtBasisPriceInBaseCurrency, exchange.exchange.exchanger, exchange.soldBoughtExchangeRate, exchange.exchange.fromCurrency, exchange.operationNumber)
          grouped.append(currency, acquisition)
        }
      case gain: Processed.Gain =>
        val currency = gain.gain.currency
        val acquisition = Acquisition(gain.gain.date, gain.gain.amount, gain.basePrice, gain.gain.exchanger, gain.basePrice, baseCurrency, gain.operationNumber)
        grouped.append(currency, acquisition)
      case composed: Processed.Composed =>
        composed.processed.foreach(process)
      case _ =>
        ;
    }
  }

  preprocess()

  private def titleFor(year: Int): String =
    s"Acquired Stocks $year"

  def printToCSVFile(path: String, year: Int): Unit =
    FileSystem.withPrintStream(path) { ps =>
      ps.println(titleFor(year))
      ps.println()
      val sep = ", "
      val header = Seq("Date Adquired","Amount","Exchanger","Cost Basis","","Exchange Rate","","Description")
      for((currency, acquisitions) <- grouped) {
        ps.println(Currency.fullName(currency))
        ps.println(header.mkString(sep))
        acquisitions.foreach{ acquisition =>
          val line = Seq(acquisition.date.format(Format.df), acquisition.amount, acquisition.exchanger
            , acquisition.costBasis, s"$baseCurrency / $currency"
            , acquisition.exchangeRate, s"${acquisition.exchangeCurrency} / $currency"
            , RichText(RichText.report(acquisition.date.getYear, acquisition.operationNumber, showYear = true)).toString
          )
          ps.println(line.mkString(sep))
        }
        ps.println()
        ps.println()
      }
    }

  def printToHTMLFile(path: String, year: Int): Unit = {
    val title = titleFor(year)
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
    var totalAcquiredAmount = 0.0
    var totalCostBasis = 0.0
    val table =
      <table id='tableStyle1'>
        <tr>
          <th></th>
          <th>Date Adquired</th>
          <th>Amount</th>
          <th class='alignL paddingL'>Exchanger</th>
          <th class='alignR paddingL'>Cost Basis</th>
          <th class='alignR paddingL'>Exchange Rate</th>
          <th class='alignL'>Description</th>
        </tr>
        <caption>
          {Currency.fullName(currency)}
        </caption>{
        grouped(currency).map { acquisition => {
        numEntries += 1
        totalAcquiredAmount += acquisition.amount
        totalCostBasis += acquisition.amount * acquisition.costBasis
        <tr>
          <td class='alignR'>
            {numEntries}
          </td>
          <td class='paddingL'>
            {Format.df.format(acquisition.date)}
          </td>
          <td class='paddingL darkBlue'>
            {Format.formatDecimal(acquisition.amount, report.decimalPlaces)}
          </td>
          <td class='exchanger alignL paddingL'>
            {acquisition.exchanger}
          </td>

          <td>
            {HTMLDoc.asRate(acquisition.costBasis, baseCurrency, currency)}
          </td>
          <td>
            {HTMLDoc.asRate(acquisition.exchangeRate, acquisition.exchangeCurrency, currency)}
          </td>
          <td>
            {RichText(RichText.report(acquisition.date.getYear, acquisition.operationNumber, showYear = true)).toHTML}
          </td>
        </tr>
      }
      }}{if (numEntries > 0)
        <tr>
          <th></th>
          <th>Total acquired:</th>
          <th>
            {Format.formatDecimal(report.showBalance(totalAcquiredAmount), report.decimalPlaces)}
          </th>
          <th>Average:</th>
          <th>
            {HTMLDoc.asRate(totalCostBasis / totalAcquiredAmount, baseCurrency, currency)}
          </th>
          <th></th>
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
