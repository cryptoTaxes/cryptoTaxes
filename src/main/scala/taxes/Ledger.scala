package taxes

import taxes.date._
import taxes.exchanger.Exchanger

import scala.collection.mutable.ListBuffer
import spray.json._
import spray.json.JsonProtocol._
import taxes.io.FileSystem

object Ledger {
  case class Entry(date: LocalDateTime, amount: Double, exchanger: Exchanger, description: RichText)

  implicit val entryJson = jsonFormat4(Entry)

  implicit object LedgerJson extends RootJsonFormat[Ledger] {
    val _currency = "currency"
    val _initialBalance = "initialBalance"
    val _entries = "entries"
    def write(ledger: Ledger) = {
      JsObject(
        _currency -> JsString(ledger.currency)
        , _initialBalance -> JsNumber(ledger._initialBalance)
        , _entries -> {
          val builder = new scala.collection.immutable.VectorBuilder[JsValue]()
          for(entry <- ledger.entries)
            builder += entry.toJson
          JsArray(builder.result())
        }
      )
    }

    def read(value: JsValue) =
      try {
        value.asJsObject.getFields(_currency, _initialBalance, _entries) match {
          case Seq(JsString(currency), JsNumber(initialBalance), JsArray(entries)) =>
            val ledger = Ledger(currency, initialBalance.doubleValue())
            entries.foreach(x => ledger.entries += x.convertTo[Entry])
            ledger
        }
      } catch {
        case _ =>
          deserializationError(s"Ledger expected in $value")
      }

    def fromFile(path: String): Ledger =
      FileSystem.withSource(path){ src =>
        src.mkString.parseJson.convertTo[Ledger]
      }
  }

  val decimalPlaces = 4.max(Config.config.decimalPlaces)

  def showBalance(balance: Double): Double =
    if(balance.abs < Config.config.epsilon) 0 else balance
}

case class Ledger(currency: Currency, private var _initialBalance: Double = 0) {
  private val entries = ListBuffer[Ledger.Entry]()

  def isEmpty: Boolean = entries.isEmpty

  def += (entry: Ledger.Entry): Unit = {
    entries += entry
  }

  def copy: Ledger = {
    val clone = Ledger(currency, _initialBalance)
    for(entry <- entries)
      clone += entry
    return clone
  }

  def initialBalance: Double = _initialBalance

  def finalBalance: Double = {
    var balance = _initialBalance
    for(entry <- entries)
      balance += entry.amount
    return balance
  }

  def prune(): Unit = {
    _initialBalance = finalBalance
    entries.clear()
  }

  def toHTML(year: Int): Option[HTML] = {
    var numEntries = 0
    var balance = _initialBalance

    val table =
      <table id='tableStyle1'>
        <tr>
          <th></th>
          <th>Date</th>
          <th class='alignR'>Amount</th>
          <th class='alignR'>Balance</th>
          <th>Exchanger</th>
          <th>Description</th>
        </tr>
        <caption>{Currency.fullName(currency)}</caption>
        <tr>
          <th></th>
          <th>Initial balance:</th>
          <th></th>
          <th class='alignR'>{report.Format.asAmount(Ledger.showBalance(_initialBalance), currency, colored = false)}</th>
          <th></th>
          <th></th>
        </tr>
        {entries.map{ entry => {
         balance += entry.amount
          if(entry.date.getYear == year) {
            numEntries += 1
            <tr>
              <td class='small1'>{numEntries}</td>
              <td>{report.Format.asDate(entry.date)}</td>
              <td class='alignR'>
                {report.Format.asAmount(entry.amount, currency, signed = true)}
              </td>
              {val bal = Ledger.showBalance(balance)
               <td class={s"alignR ${
                  if(bal < 0) "darkRed"
                  else if(bal == 0) "darkMagenta"
                  else ""}"
               }>
                 {report.Format.asAmount(bal,currency, colored = false)}
               </td>
              }
              <td class='exchanger'>{entry.exchanger}</td>
              <td class='alignR small1'>{entry.description.toHTML}</td>
            </tr>
          }
        }}
        }
        {if(numEntries>0)
          <tr>
            <th></th>
            <th>Final balance:</th>
            <th></th>
            <th class='alignR'>{report.Format.asAmount(Ledger.showBalance(balance), currency, colored = false)}</th>
            <th></th>
            <th></th>
          </tr>
        }
      </table>
    if(numEntries>0)
      Some{<span id={currency}>{table}</span>}
    else
      None
  }

  def saveToFile(path: String): Unit = {
    FileSystem.withPrintStream(path) {
      _.println(this.toJson.prettyPrint)
    }
  }
}

object LedgerPool {
  implicit object LedgerPoolJson extends RootJsonFormat[LedgerPool] {
    val _id = "id"
    val _ledgers = "ledgers"

    def write(ledgerPool: LedgerPool) = {
      JsObject(
        _id -> JsString(ledgerPool.id)
        , _ledgers -> {
          val builder = new scala.collection.immutable.VectorBuilder[JsValue]()
          for ((currency, ledger) <- ledgerPool.ledgers)
            builder += ledger.toJson
          JsArray(builder.result())
        }
      )
    }

    def read(value: JsValue) =
      try {
        value.asJsObject.getFields(_id, _ledgers) match {
          case Seq(JsString(id), JsArray(ledgers)) =>
            val ledgerPool = LedgerPool(id)
            for(json <- ledgers) {
              val ledger = json.convertTo[Ledger]
              ledgerPool.ledgers(ledger.currency) = ledger
            }
            ledgerPool
        }
      } catch {
        case _ =>
          deserializationError(s"LedgerPool expected in $value")
      }
  }

  def fromFile(path: String): LedgerPool =
    FileSystem.withSource(path){ src =>
      src.mkString.parseJson.convertTo[LedgerPool]
    }
}

case class LedgerPool(id: String) extends Iterable[Ledger] {
  private val ledgers = scala.collection.mutable.Map[Currency, Ledger]()

  def iterator: Iterator[Ledger] =
    ledgers.iterator.map(_._2)

  def delete(currency: Currency): Unit = {
    ledgers -= currency
  }

  def record(currency: Currency)(date: LocalDateTime, amount: Double, exchanger: Exchanger, description: RichText): Unit = {
    val ledger = ledgers.getOrElse(currency, Ledger(currency))
    ledger += Ledger.Entry(date, amount, exchanger, description)
    ledgers(currency) = ledger
  }

  def saveToDisk(path: String): Unit = {
    FileSystem.withPrintStream(FileSystem.compose(Seq(path), id, FileSystem.ledgerPoolExtension)) {
      _.println(this.toJson.prettyPrint)
    }
  }

  def prune(): Unit = {
    for((currency, ledger) <- ledgers)
      ledger.prune()
  }

  def readFromFile(path: String): Unit = {
    val filePath = FileSystem.compose(Seq(path), id, FileSystem.ledgerPoolExtension)

    ledgers.clear()
    if(FileSystem.File(filePath).exists()) {
      val ledgerPool = LedgerPool.fromFile(filePath)
      for((currency, ledger) <- ledgerPool.ledgers)
        ledgers(currency) = ledger
    }
  }

  def toHTML(year: Int): Option[HTML] =
    if(ledgers.isEmpty || ledgers.forall{case (currency, ledger) => ledger.isEmpty})
      None
    else
      Some {
        <div>{
          ledgers.toList.sortBy(_._1).map{
            case (currency, ledger) =>
              ledger.toHTML(year) match {
                case None => <div></div>
                case Some(html) => html
              }
            }
          }
        </div>
      }

  def summaryToHTML(beginOfYear: Boolean = false): Option[HTML] = {
    def balance(ledger: Ledger) =
      if(beginOfYear) ledger.initialBalance else ledger.finalBalance
    val nonEmpty = this.exists(balance(_) != 0)
    if(nonEmpty)
      Some{
        <span id={id}>
          <table id='tableStyle1'>
            <tr>
              <th>Currency</th>
              <th>Balance</th>
            </tr>
            <caption>
              {id}
            </caption>{this.toList.filter(balance(_) != 0).sortBy(_.currency).map { ledger: Ledger =>
            <tr>
              <td>
                <span class='currency'>
                  {ledger.currency}
                </span>
              </td>
              <td>
                {Format.formatDecimal(Ledger.showBalance(balance(ledger)), Ledger.decimalPlaces)}
              </td>
            </tr>
          }}
          </table>
        </span>}
    else
      None
  }
}