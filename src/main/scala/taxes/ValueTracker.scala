package taxes

case class ValueTracker(baseMarket: Market) extends Iterable[(Market, Double)] with ToHTML {
  private val map = scala.collection.mutable.Map[Market,Double]()

  def clear(): Unit =
    map.clear()

  def record(market: Market, amount: Double): Unit =
    map += (market -> (amount + map.getOrElse(market, 0.0)))

  def sum: Double =
    map.values.sum

  def keys: Iterable[Market] =
    map.keys

  def iterator: Iterator[(Market, Double)] =
    map.iterator

  def apply(market: Market): Double =
    map.getOrElse(market, 0.0)

  override def toHTML: HTML =
    toHTML()

  def toHTML(caption: String = ""): HTML =
    <table id='tableStyle1'>
      <tr>
        <th>Market</th>
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
        <td ><span class='market'>{label}</span></td>
        <td>{HTMLDoc.asMarket(total, baseMarket)}</td>
        <td>{HTMLDoc.asMarket(sum, baseMarket)}</td>
      </tr>
    }
      }
      <td class='embold'>Total:</td>
      <td></td>
      <td>{HTMLDoc.asMarket(map.values.sum, baseMarket)}</td>
    </table>
}
