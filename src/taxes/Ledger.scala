package taxes

import taxes.date._
import taxes.exchanger.Exchanger

import scala.collection.mutable.ListBuffer
import spray.json._
import DefaultJsonProtocol._

object Ledger {
  case class Entry(date : LocalDateTime, amount : Double, exchanger: Exchanger, description : String)
  implicit val entryJson = jsonFormat4(Entry)

  implicit object LedgerTimeJson extends RootJsonFormat[Ledger] {
    def write(ledger: Ledger) = {
      JsObject(
        "market" -> JsString(ledger.market)
        , "initialBalance" -> JsNumber(ledger.initialBalance)
        , "entries" -> {
          val builder = new scala.collection.immutable.VectorBuilder[JsValue]()
          for(entry <- ledger.entries)
            builder += entry.toJson
          JsArray(builder.result())
        }
      )
    }

    def read(value: JsValue) = value match {
      case Seq(JsString(market), JsNumber(initialBalance), JsArray(entries)) =>
        val ledger = Ledger(market, initialBalance.doubleValue())
        entries.foreach(x => ledger.entries += x.convertTo[Entry])
        ledger
      case _ =>
        deserializationError("Ledger expected")
    }
  }
}

case class Ledger(market : Market, var initialBalance : Double = 0) {
  private val entries = ListBuffer[Ledger.Entry]()

  def += (entry: Ledger.Entry): Unit = {
    entries += entry
  }

  def finalBalance : Double = {
    var balance = initialBalance
    for(entry <- entries)
      balance += entry.amount
    return balance
  }

  def toHTML(year : Int) : Option[HTML] = {
    var numEntries = 0
    var balance = initialBalance
    val fmt = "%.8f"

    val table =
      <table id='tableStyle1'>
        <tr>
          <th></th>
          <th>Date</th>
          <th>Amount</th>
          <th>Balance</th>
          <th class='alignL paddingL'>Exchanger</th>
          <th class='alignL'>Description</th>
        </tr>
        <caption>{market}</caption>
        {entries.map{ entry => {
         balance += entry.amount
          if(entry.date.getYear == year) {
            numEntries += 1
            <tr>
              <td class='alignR'>{numEntries}</td>
              <td class='paddingL'>{Format.df.format(entry.date)}</td>
              <td class={"paddingL" + (if (entry.amount<0) " darkRed" else " darkBlue")}>
                {fmt.format(entry.amount)}
              </td>
              <td class='paddingL'>{fmt.format(balance)}</td>
              <td class='exchanger alignL paddingL'>{entry.exchanger}</td>
              <td class='alignL'>{entry.description}</td>
            </tr>
          }
        }}
        }
        {if(numEntries>0)
          <tr>
            <th></th>
            <th>Final balance:</th>
            <th></th>
            <th>{fmt.format(balance)}</th>
            <th></th>
            <th></th>
          </tr>
        }
      </table>
    if(numEntries>0)
      Some{table}
    else
      None
  }

  def toFile(path : String): Unit = {
    FileSystem.withPrintStream(path) {
      _.println(this.toJson.prettyPrint)
    }
  }
}
