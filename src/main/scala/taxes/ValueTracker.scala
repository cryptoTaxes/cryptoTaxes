package taxes

import taxes.report.Format.asCurrency

case class ValueTracker(baseCurrency: Currency) extends Iterable[(Currency, Double)] with ToHTML {
  private val map = scala.collection.mutable.Map[Currency,Double]()

  def clear(): Unit =
    map.clear()

  def record(currency: Currency, amount: Double): Unit =
    map += (currency -> (amount + map.getOrElse(currency, 0.0)))

  def sum: Double =
    map.values.sum

  def keys: Iterable[Currency] =
    map.keys

  def iterator: Iterator[(Currency, Double)] =
    map.iterator

  def apply(currency: Currency): Double =
    map.getOrElse(currency, 0.0)

  override def toHTML: HTML =
    toHTML()

  def toHTML(caption: String = ""): HTML =
    <table id='tableStyle1'>
      <tr>
        <th>Currency</th>
        <th>Amount</th>
        <th>Accumulated</th>
      </tr>
      {if(caption.nonEmpty)
      <caption>{caption}</caption>
      }
      {var sum = 0.0
    map.toList.sortBy(_._2).map { case (label,total) =>
      sum += total
      <tr>
        <td ><span class='currency'>{label}</span></td>
        <td>{asCurrency(total, baseCurrency)}</td>
        <td>{asCurrency(sum, baseCurrency)}</td>
      </tr>
    }
      }
      <td class='embold'>Total:</td>
      <td></td>
      <td>{asCurrency(map.values.sum, baseCurrency)}</td>
    </table>
}
