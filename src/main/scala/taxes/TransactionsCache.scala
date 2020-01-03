package taxes

import taxes.date._
import taxes.io.FileSystem
import taxes.util.Logger

import spray.json._
import spray.json.JsonProtocol._

object TransactionsCache extends Initializable with Finalizable {
  case class TxKey(currency: Currency, txid: String, address: String)
  implicit val txKeyJson = jsonFormat3(TxKey)

  case class TxInfo(amount: Double, fee: Double, localDate: LocalDateTime)
  implicit val txInfoJson = jsonFormat3(TxInfo)

  private val map = scala.collection.mutable.Map[TxKey, TxInfo]()

  private val file = FileSystem.File(FileSystem.transactionsCacheFile)

  private var modified = false

  def saveToDisk(): Unit = {
    FileSystem.withPrintStream(file){ ps =>
      spray.json.PrintStream.prettyPrintOn(ps, map)
    }
  }

  def loadFromDisk(): Unit = {
    if(!file.exists())
      return

    val pairs = FileSystem.withSource(file){ src =>
      src.mkString.parseJson.convertTo[List[(TxKey, TxInfo)]]
    }

    for(pair <- pairs)
      map += pair
  }

  def lookup(currency: Currency, txid: String, address: String): TxInfo = {
    map.get(TxKey(currency, txid, address)) match {
      case Some(txInfo) => txInfo
      case None =>
        val searcher = BlockExplorer.Searcher(currency, txid, address)
        searcher.search match {
          case Some(_) =>
            val key = TxKey(currency, txid, address)
            val info = TxInfo(searcher.amount, searcher.fee, searcher.date)
            map += (key -> info)
            modified = true
            return info
          case None =>
            Logger.fatal(s"TransactionCache not implemented yet for $currency. $txid.")
        }
    }
  }

  override def init(): Unit = {
    loadFromDisk()
  }

  override def wrapUp(): Unit = {
    if(modified)
      saveToDisk()
  }
}
