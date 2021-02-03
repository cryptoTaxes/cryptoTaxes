package taxes.report

import taxes.{HTMLDoc, StockPool}
import taxes.exchanger.Exchanger

case class Ledgers(allStocks: StockPool, allExchangers: List[Exchanger], year: Int) {
  def printToHTMLFile(path: String): Unit = {
    val title = s"Ledgers $year"
    val htmlDoc = HTMLDoc(path, title)

    htmlDoc += <div class='header'>{title}</div>

    htmlDoc += <div class='header'>Spot currencies</div>
    for(stock <- allStocks.toList.sortBy(_.currency))
      stock.ledger.toHTML(year) match {
        case None => ;
        case Some(html) => htmlDoc += html
      }

    for(exchanger <- allExchangers)
      for((stockPool, title) <- List((exchanger.marginLongs, "Margin Longs"), (exchanger.marginShorts, "Margin Shorts"))) {
        val htmls = stockPool.toList.sortBy(_.id).flatMap(_.ledger.toHTML(year))

        if(htmls.nonEmpty) {
          htmlDoc += <div class='header'>{exchanger + "  " + title}</div>
          for(html <- htmls)
            htmlDoc += html
        }
      }
    htmlDoc.close()
  }
}