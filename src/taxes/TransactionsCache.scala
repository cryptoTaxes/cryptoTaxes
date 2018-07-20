package taxes

import java.io.File

import taxes.date._
import taxes.util.Logger
import spray.json._
import DefaultJsonProtocol._

object TransactionsCache extends Initializable with Finalizable {
  case class TxKey(market: Market, txid : String, address : String)
  implicit val txKeyJson = jsonFormat3(TxKey)

  case class TxInfo(amount : Double, fee : Double, localDate : LocalDateTime)
  implicit val txInfoJson = jsonFormat3(TxInfo)

  private val map = scala.collection.mutable.Map[TxKey, TxInfo]()

  private val file = new File(FileSystem.transactionsCacheFile)

  def saveToDisk(): Unit = {
    FileSystem.withPrintStream(file){
      _.println(map.toList.toJson.prettyPrint)
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
      case None => {
        if (List(Market.bitcoin, Market.litecoin, Market.dogecoin, Market.etc, Market.vertcoin).contains(market)) {
          val sc = BlockExplorerSearcher(market, txid, address)
          val key = TxKey(market, txid, address)
          val info = TxInfo(sc.amount, sc.fee, sc.date)
          map += (key -> info)
          saveToDisk()
          return info
        } else {
          Logger.fatal("TransactionCache not implemented yet for %s. %s.".format(market, txid))
        }
      }
    }
  }

  override def init(): Unit = {
    loadFromDisk()
  }

  override def wrapUp(): Unit = {
    saveToDisk()
  }
}
