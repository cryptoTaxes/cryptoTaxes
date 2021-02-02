package taxes

case class AcquiredStocks(currency: Currency, baseCurrency: Currency) extends Queue[Stock] {
  def toHTML: Option[HTML] = {
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
        </caption>{map { stock => {
        numEntries += 1
        totalAcquiredAmount += stock.amount
        totalCostBasis += stock.amount * stock.costBasis
        <tr>
          <td class='alignR'>
            {numEntries}
          </td>
          <td class='paddingL'>
            {Format.df.format(stock.date)}
          </td>
          <td class='paddingL darkBlue'>
            {Format.formatDecimal(stock.amount, DisposedStocksQueue.decimalPlaces)}
          </td>
          <td class='exchanger alignL paddingL'>
            {stock.exchanger}
          </td>

          <td>
            {HTMLDoc.asRate(stock.costBasis, baseCurrency, currency)}
          </td>
          <td>
            {HTMLDoc.asRate(stock.exchangeRate, stock.exchangeCurrency, currency)}
          </td>
          <td>
            {RichText(RichText.report(stock.date.getYear, stock.operationNumber, showYear = true)).toHTML}
          </td>
        </tr>
      }
      }}{if (numEntries > 0)
        <tr>
          <th></th>
          <th>Total acquired:</th>
          <th>
            {Format.formatDecimal(Ledger.showBalance(totalAcquiredAmount), DisposedStocksQueue.decimalPlaces)}
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
