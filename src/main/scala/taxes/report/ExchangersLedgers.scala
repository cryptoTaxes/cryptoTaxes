package taxes.report

import taxes.HTMLDoc
import taxes.exchanger.Exchanger
import taxes.io.FileSystem

case class ExchangersLedgers(allExchangers: List[Exchanger], year: Int) {

    def printToHTMLFile(): Unit =
      allExchangers.foreach(printToHTMLFile)

    def printToHTMLFile(exchanger: Exchanger): Unit = {
      val htmlExchangerLedgersFile = s"${FileSystem.userOutputFolder(year)}/Exchanger.$exchanger.Ledgers.$year.html"
      val title = s"$exchanger Ledgers $year"
      val htmlDoc = HTMLDoc(htmlExchangerLedgersFile, title)

      exchanger.ledgerPool.toHTML(year) match {
        case None => ;
        case Some(html) =>
          htmlDoc += <div class='header'>{title}</div>
          htmlDoc += html
      }

      for((marginStockPool, subtitle) <- List((exchanger.marginLongs, "Margin Longs"), (exchanger.marginShorts, "Margin Shorts"))) {
        val htmls = marginStockPool.toList.sortBy(_.id).flatMap(_.ledger.toHTML(year))

        if(htmls.nonEmpty) {
          htmlDoc += <div class='header'>{subtitle}</div>
          for(html <- htmls)
            htmlDoc += html
        }
      }
      htmlDoc.close()
  }
}
