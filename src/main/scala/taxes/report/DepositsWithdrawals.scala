package taxes.report

import taxes.{Currency, Deposit, HTML, HTMLDoc, Operation, RichText, Withdrawal, report}
import taxes.collection.Grouped
import taxes.io.FileSystem

case class DepositsWithdrawals(operations: Seq[Operation], year: Int) {
  private val grouped = Grouped[Currency, Operation]()

  preprocess()

  private def preprocess(): Unit =
    operations.filter(_.date.getYear == year).foreach(process)

  private def process(operation: Operation): Unit = {
    operation match {
      case deposit: Deposit =>
        grouped.append(deposit.currency, deposit)
      case withdrawal: Withdrawal =>
        grouped.append(withdrawal.currency, withdrawal)
      case _ =>
        ;
    }
  }

  private val title =
    s"Exchangers Deposits and Withdrawals $year"

  private def defaultFile(ext: String): String =
    s"${FileSystem.userOutputFolder(year)}/DepositsWithdrawals.$year.$ext"

  def printToCSVFile(): Unit =
    printToCSVFile(defaultFile("csv"))

  def printToCSVFile(path: String): Unit = {}

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

  private def description(currency: Currency, txidOpt: Option[String], addressOpt: Option[String]): HTML = {
    (txidOpt, addressOpt) match {
      case (None, None) =>
        <span></span>
      case (Some(txid), Some(address)) =>
        RichText(RichText.util.transaction(currency, txid, address)).toHTML
      case (Some(txid), None) =>
        RichText(RichText.util.transaction(currency, txid)).toHTML
      case (None, Some(address)) =>
        RichText(RichText.util.address(currency, address)).toHTML
    }
  }

  private def toHTML(operation: Operation): HTML = operation match {
    case deposit: Deposit =>
      <tr>
        <td>{report.Format.asDate(deposit.date)}</td>
        <td>Deposit</td>
        <td class='alignR'>
          {report.Format.asAmount(deposit.amount, deposit.currency, signed = true)}
        </td>
        <td class='exchanger'>
          {deposit.exchanger}
        </td>
        <td class='small1'>
          {(RichText(s"Deposit ")+deposit.description+RichText.nl+RichText.util.transaction(deposit.currency, deposit.txid, deposit.address)).toHTML}
        </td>
      </tr>

    case withdrawal: Withdrawal =>
      <tr>
        <td>{report.Format.asDate(withdrawal.date)}</td>
        <td>Withdrawal</td>
        <td class='alignR'>
          {report.Format.asAmount(-withdrawal.amount, withdrawal.currency, signed = true)}
        </td>
        <td class='exchanger'>
          {withdrawal.exchanger}
        </td>
        <td class='small1'>
          {(RichText(s"Withdrawal ")+withdrawal.description+RichText.nl+RichText.util.transaction(withdrawal.currency, withdrawal.txid, withdrawal.address)).toHTML}
        </td>
      </tr>
  }

  private def toHTML(currency: Currency): Option[HTML] = {
    val operations = grouped(currency)
    val table =
      <table id='tableStyle1'>
        <tr>
          <th>Date</th>
          <th>What</th>
          <th class='alignR'>Amount</th>
          <th class='alignR'>Exchanger</th>
          <th>Reference</th>
        </tr>
        <caption>
          {Currency.fullName(currency)}
        </caption>{
        val operations = grouped(currency)
        operations.map(toHTML)
        }
      </table>
    if (operations.nonEmpty)
      Some {
        <span id={currency}>
          {table}
        </span>
      }
    else
      None
  }
}
