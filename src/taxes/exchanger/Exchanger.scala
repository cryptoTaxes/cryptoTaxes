package taxes.exchanger

import taxes._
import taxes.io.FileSystem
import taxes.util.Logger

import scala.collection.mutable.ListBuffer


trait Exchanger extends Initializable {
  val id : String

  override def toString: String = id

  val sources : Seq[Source[Operation]]
}

object Exchanger {
  val allExchangers : List[Exchanger] =
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
      , Kraken
      , LocalBTC
      , Poloniex
      , Shapeshift
      , RippleTrade
      , Yobit
      )

  def readAllOperations(): Seq[Operation] = {
    val operations = ListBuffer[Operation]()
    for (exchanger <- allExchangers) {
      Logger.trace(s"Reading data for $exchanger.")
      for (src <- exchanger.sources)
        operations ++= src.read()
    }
    return operations
  }

  private lazy val stringToExchanger = allExchangers.filter(_ != General).map(exch => (exch.toString, exch)).toMap

  def parse(str : String) : Exchanger =
    stringToExchanger.getOrElse(str, General(str))

  import spray.json.{JsonFormat, JsString, JsValue, deserializationError}

  object exchangerJson extends JsonFormat[Exchanger] {
    def write(exchanger: Exchanger) = {
      JsString(exchanger.toString)
    }

    def read(value: JsValue) = value match {
      case JsString(str) =>
        Exchanger.parse(str)
      case _ =>
        deserializationError("Exchanger expected")
    }
  }
}


abstract case class UserInputFolderSource[+A](folderPath : String, extension : String) extends FolderSource[A](s"${FileSystem.userInputFolder}/$folderPath", extension)
