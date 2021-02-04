package taxes.report

import taxes.{HTMLDoc, StockPool}
import taxes.exchanger.Exchanger
import taxes.io.FileSystem

case class Portfolio(allStocks: StockPool, year: Int, beginOfYear: Boolean = false) {
  private val when = if(beginOfYear) "Beginning" else "End"
  private val title = s"$year $when of year portfolio"

  def printToHTMLFile(): Unit = {
    val htmlPortfolioFile = s"${FileSystem.userOutputFolder(year)}/Portfolio.$when.$year.html"
    val htmlPortfolioTitle = title
    val htmlDoc = HTMLDoc(htmlPortfolioFile, htmlPortfolioTitle)

    htmlDoc += allStocks.toHTML(htmlPortfolioTitle)

    for(exchanger <- Exchanger.allExchangers.sortBy(_.id))
      exchanger.ledgerPool.summaryToHTML(beginOfYear) match {
        case None => ;
        case Some(html) => htmlDoc += html
      }

    htmlDoc.close()

  }

  def printToCSVFile(): Unit = {
    val csvFileName = FileSystem.userOutputFolder(year) + s"/Portfolio.$when.$year.csv"
    allStocks.printToCSVFile(csvFileName, year, title)
  }
}

