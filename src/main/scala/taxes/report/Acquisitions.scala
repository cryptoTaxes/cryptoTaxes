package taxes.report

import taxes.collection.Grouped
import taxes.{Config, Currency, HTML, HTMLDoc, Processed, RichText, report}
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
    , reference: RichText
    )
}

case class Acquisitions(baseCurrency: Currency, processedOperations: Seq[Processed], year: Int) {
  import Acquisitions._

  private val grouped = Grouped[Currency, Acquisition]()

  preprocess()

  private def preprocess(): Unit =
    processedOperations.foreach(process)

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
          val acquisition = Acquisition(
            exchange.exchange.date
            , amount
            , exchange.boughtBasisPriceInBaseCurrency
            , exchange.exchange.exchanger
            , exchange.soldBoughtExchangeRate
            , exchange.exchange.fromCurrency
            , RichText(s"Exchange ${RichText.report(exchange.exchange.date.getYear, exchange.operationNumber, showYear = false)}")
          )
          grouped.append(currency, acquisition)
        }

      case gain: Processed.Gain =>
        val currency = gain.gain.currency
        val acquisition = Acquisition(
          gain.gain.date
          , gain.gain.amount
          , gain.basePrice
          , gain.gain.exchanger
          , gain.basePrice
          , baseCurrency
          , RichText(s"Gain ${RichText.report(gain.gain.date.getYear, gain.operationNumber, showYear = false)}")
        )
        grouped.append(currency, acquisition)

      case composed: Processed.Composed =>
        composed.processed.foreach(process)

      case _ =>
        ;
    }
  }

  private val title =
    s"Acquired Stocks $year"

  private def defaultFile(ext: String): String =
    s"${FileSystem.userOutputFolder(year)}/AcquiredStocks.$year.$ext"

  def printToCSVFile(): Unit =
    printToCSVFile(defaultFile("csv"))

  def printToCSVFile(path: String): Unit =
    FileSystem.withPrintStream(path) { ps =>
      ps.println(title)
      ps.println()
      val sep = ", "
      val header = Seq("Date Acquired","Amount","Exchanger","Cost Basis","","Exchange Rate","","Reference")
      for((currency, acquisitions) <- grouped) {
        ps.println(Currency.fullName(currency))
        ps.println(header.mkString(sep))
        acquisitions.foreach{ acquisition =>
          val line = Seq(acquisition.date.format(taxes.Format.df)
            , acquisition.amount
            , acquisition.exchanger
            , acquisition.costBasis
            , s"$baseCurrency / $currency"
            , acquisition.exchangeRate
            , s"${acquisition.exchangeCurrency} / $currency"
            , acquisition.reference.toString
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
    var totalAcquiredAmount = 0.0
    var totalCostBasis = 0.0
    val table =
      <table id='tableStyle1'>
        <tr>
          <th></th>
          <th>Date Acquired</th>
          <th class='alignR'>Amount</th>
          <th>Exchanger</th>
          <th class='alignR'>Cost Basis</th>
          <th class='alignR'>Exchange Rate</th>
          <th class='alignR'>Reference</th>
        </tr>
        <caption>
          {Currency.fullName(currency)}
        </caption>{
        grouped(currency).map { acquisition => {
        numEntries += 1
        totalAcquiredAmount += acquisition.amount
        totalCostBasis += acquisition.amount * acquisition.costBasis
        <tr>
          <td class='alignR small1'>
            {numEntries}
          </td>
          <td>
            {report.Format.asDate(acquisition.date)}
          </td>
          <td class='alignR'>
            {report.Format.asAmount(acquisition.amount, currency)}
          </td>
          <td class='exchanger'>
            {acquisition.exchanger}
          </td>
          <td class='alignR'>
            {report.Format.asRate(acquisition.costBasis, baseCurrency, currency)}
          </td>
          <td class='alignR'>
            {report.Format.asRate(acquisition.exchangeRate, acquisition.exchangeCurrency, currency)}
          </td>
          <td class='small1'>
            {acquisition.reference.toHTML}
          </td>
        </tr>
      }
      }}{if (numEntries > 0)
        <tr>
          <th></th>
          <th>Total acquired:</th>
          <th class='alignR'>
            {report.Format.asAmount(totalAcquiredAmount, currency)}
          </th>
          <th>Average:</th>
          <th class='alignR'>
            {report.Format.asRate(totalCostBasis / totalAcquiredAmount, currency, baseCurrency)}
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
