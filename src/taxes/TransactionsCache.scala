package taxes

import taxes.date._
import taxes.io.FileSystem
import taxes.util.Logger

import spray.json._
import spray.json.JsonProtocol._

object TransactionsCache extends Initializable with Finalizable {
  case class TxKey(market: Market, txid : String, address : String)
  implicit val txKeyJson = jsonFormat3(TxKey)

  case class TxInfo(amount : Double, fee : Double, localDate : LocalDateTime)
  implicit val txInfoJson = jsonFormat3(TxInfo)

  private val map = scala.collection.mutable.Map[TxKey, TxInfo]()

  private val file = FileSystem.File(FileSystem.transactionsCacheFile)

  private var modified = false

  def saveToDisk(): Unit = {
    FileSystem.withPrintStream(file){ ps =>
      ps.println(map.toList.toJson.prettyPrint)
    }
  }

  def loadFromDisk(): Unit = {
    if(!file.exists())
      return

    val pairs = FileSystem.withSource(file){ src =>
      JsonParser(src.mkString).convertTo[List[(TxKey, TxInfo)]]
    }

    for(pair <- pairs)
      map += pair
  }

  def lookup(market: Market, txid : String, address : String) : TxInfo = {
    map.get(TxKey(market, txid, address)) match {
      case Some(txInfo) => txInfo
      case None =>
        val searcher = BlockExplorerSearcher(market, txid, address)
        searcher.search match {
          case Some(_) =>
            val key = TxKey(market, txid, address)
            val info = TxInfo(searcher.amount, searcher.fee, searcher.date)
            map += (key -> info)
            modified = true
            return info
          case None =>
            Logger.fatal(s"TransactionCache not implemented yet for $market. $txid.")
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
