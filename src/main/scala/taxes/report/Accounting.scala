package taxes.report

import taxes.{Accounting, Config, Currency, HTML, HTMLDoc, OperationTracker, Processed, Report}
import taxes.io.FileSystem
import taxes.report.Format.asCurrency

case class Accounting(baseCurrency: Currency, processedOperations: Seq[Processed], realized: Report.Realized, operationTracker: OperationTracker, year: Int) {
  private val method = taxes.Accounting.toString(Config.config.accountingMethod)

  def printToHTMLFile(): Unit = {
    val htmlReportFile = FileSystem.report(year, "html")
    val htmlReportTitle = s"$year $method Report"
    val htmlDoc = HTMLDoc(htmlReportFile, htmlReportTitle)

    htmlDoc += <div class='header'>
      {htmlReportTitle}
    </div>

    for (processed <- processedOperations)
      htmlDoc += processed

    htmlDoc += reportResults(year, realized)
    htmlDoc.close()
  }

  private def reportResults(year: Int, realized: Report.Realized): HTML = {
    val proceeds = realized.proceeds.sum
    val costs = realized.costBasis.sum
    val fees = realized.perCurrencyPaidFees.sum

    val net = proceeds - costs - fees
    val netMsg = s"Net ${if(net > 0) "gain" else "loss"}:"
    <table id='tableStyle1'>
      <caption>{s"$year Resume"}</caption>
      <tr>
        <td><span class='embold'>Total proceeds:</span></td>
        <td class='alignR'>{asCurrency(proceeds, baseCurrency)}</td>
      </tr>
      <tr>
        <td><span class='embold'>Total cost bases:</span></td>
        <td class='alignR'>{asCurrency(costs, baseCurrency)}</td>
      </tr>
      <tr>
        <td><span class='embold'>Total fees:</span></td>
        <td class='alignR'>{asCurrency(fees, baseCurrency)}</td>
      </tr>
      <tr>
        <td><span class='embold'>{netMsg}</span></td>
        <td class='alignR'>{asCurrency(net, baseCurrency)}</td>
      </tr>
    </table>
  }



  def printToCSVFile(): Unit = {
    val csvReportFile = FileSystem.report(year, "csv")
    operationTracker.printToCSVFile(csvReportFile, year, baseCurrency)
  }
}
