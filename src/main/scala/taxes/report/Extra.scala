package taxes.report

import taxes.{Config, Currency, Exchange, HTML, HTMLDoc, Report, ValueTracker, Verbosity}
import taxes.Report.Realized
import taxes.exchanger.Exchanger
import taxes.io.FileSystem
import taxes.report.Format.asCurrency

case class Extra(baseCurrency: Currency, partiallyFrees: Seq[String], frees: Seq[Exchange], realized: Realized, year: Int) {
  def printToHTMLFile(): Unit = {
    val htmlExtraFile = s"${FileSystem.userOutputFolder(year)}/Extra.$year.html"
    val htmlExtraTitle = s"$year Statistics"
    val htmlDoc = HTMLDoc(htmlExtraFile, htmlExtraTitle)

    htmlDoc += <div class='header'>
      {htmlExtraTitle}
    </div>
    htmlDoc += reportYear(year, realized)

    if (Config.verbosity(Verbosity.showMoreDetails)) {
      {
        htmlDoc += <div>Frees:</div>
      }
      <div>
        {for (f <- partiallyFrees)
        htmlDoc += <div>
          {f}
        </div>}
      </div>

      {
        htmlDoc += <div>Priced0:</div>
      }
      <div>
        {for (op <- frees)
        htmlDoc += <div>
          {op}
        </div>}
      </div>

      {htmlDoc += <div>Opened margin longs:</div>}

      <div>
        {for (stockPool <- Exchanger.allExchangers.map(_.marginLongs))
        for (stock <- stockPool)
          if (stock.totalAmount > 0)
            htmlDoc += <div>
              {stock.toHTML(showTotal = true)}
            </div>}
      </div>

      {htmlDoc += <div>Opened margin shorts:</div>}

      <div>
        {for (stockPool <- Exchanger.allExchangers.map(_.marginShorts))
        for (stock <- stockPool)
          if (stock.totalAmount > 0)
            htmlDoc += <div>
              {stock.toHTML(showTotal = true)}
            </div>}
      </div>
    }
    htmlDoc.close()
  }

  private def reportYear(year: Int, realized: Report.Realized): HTML = {
    <div>
      <div>{realized.perCurrencyGains.toHTML("Gains per currency")}</div>
      <div>{realized.perCurrencyLooses.toHTML("Looses per currency")}</div>
      <div>{realized.perCurrencyPaidFees.toHTML("Paid fees per currency")}</div>
      <div class='marginTopBottom20'>
        <span class='embold'>Net result:</span>
        {asCurrency(realized.perCurrencyGains.sum - realized.perCurrencyLooses.sum - realized.perCurrencyPaidFees.sum, baseCurrency)}
      </div>

      <div>{realized.costBasis.toHTML("Cost bases per currency")}</div>
      <div>{realized.proceeds.toHTML("Proceeds per currency")}</div>
        { val realizedGains = {
          val keys = realized.costBasis.keys.toSet union realized.proceeds.keys.toSet
          val list = keys.map(k => (k, realized.proceeds(k) - realized.costBasis(k)))
          val valueTracker = ValueTracker(baseCurrency)
          for((k,v) <- list)
            valueTracker.record(k,v)
          valueTracker
          }
        <div>{realizedGains.toHTML("Realized Gains per currency")}</div>
        }
      <div class='marginTopBottom20'>
        <span class='embold'>Net result:</span>
        {asCurrency(realized.proceeds.sum - realized.costBasis.sum - realized.perCurrencyPaidFees.sum, baseCurrency)}
      </div>
    </div>
  }
}
