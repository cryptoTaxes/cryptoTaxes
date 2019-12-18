package taxes.exchanger

import taxes._
import taxes.io.FileSystem
import taxes.util.Logger

import scala.collection.mutable.ListBuffer

import spray.json._

trait Exchanger {
  val id: String

  override def toString: String = id

  val sources: Seq[Source[Operation]]

  lazy val ledgerPool: LedgerPool = LedgerPool(id)  // lazy as it depends on id being assigned

  // current in longs/shorts assets for margin trading operations
  val marginLongs: StockPool = QueueStockPool()
  val marginShorts: StockPool = QueueStockPool()

  def saveToDisk(year: Int): Unit = {
    for((stockPool, isLong) <- List((marginLongs, true), (marginShorts, false))) {
      val path = FileSystem.exchangerMarginFolder(year, this, isLong)
      stockPool.saveToDisk(path)
    }

    val path = FileSystem.exchangerLedgersFolder(year, this)
    ledgerPool.saveToDisk(path)
  }

  def loadFromDisk(year: Int): Unit = {
    for((stockPool, isLong) <- List((marginLongs, true), (marginShorts, false))) {
      val path = FileSystem.exchangerMarginFolder(year, this, isLong)
      stockPool.loadFromDisk(path)
      for(stock <- stockPool)
        stock.ledger.prune()
    }

    val path = FileSystem.exchangerLedgersFolder(year, this)
    ledgerPool.readFromFile(path)
    ledgerPool.prune()
  }
}

object Exchanger {
  val allExchangers: List[Exchanger] =
    List[Exchanger](
        Binance
      , Bitfinex
      , Bittrex
      , CCEX
      , Changelly
      , Coinbase
      , General
      , GDAX
      , HitBTC
      , Idex
      , Kraken
      , Kucoin
      , LocalBTC
      , Poloniex
      , Shapeshift
      , RippleTrade
      , Yobit
      )

  def preprocessAndReadAllSources(): Seq[Operation] = {
    for(exchanger <- allExchangers) {
      Logger.trace(s"Preprocessing data for $exchanger.")
      for(src <- exchanger.sources)
        src.preprocess() match {
          case None =>
            ;
          case Some(procedure) =>
            procedure()
        }
    }

    val operations = ListBuffer[Operation]()
    for(exchanger <- allExchangers) {
      Logger.trace(s"Reading data for $exchanger.")
      for(src <- exchanger.sources)
        operations ++= src.read()
    }
    return operations
  }

  private lazy val stringToExchanger = allExchangers.filter(_ != General).map(exch => (exch.toString, exch)).toMap

  def parse(str: String): Exchanger =
    stringToExchanger.getOrElse(str, General)

    object exchangerJson extends JsonFormat[Exchanger] {
    def write(exchanger: Exchanger) = {
      JsString(exchanger.toString)
    }

    def read(value: JsValue) =
      try {
        value match {
          case JsString(str) =>
            Exchanger.parse(str)
        }
      } catch {
        case _ =>
          deserializationError(s"Exchanger expected in $value")
      }
  }
}


abstract case class UserInputFolderSource[+A](folderPath: String, extension: String) extends FolderSource[A](s"${FileSystem.userInputFolder}/$folderPath", extension)
