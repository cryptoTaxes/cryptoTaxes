package taxes.Exchanger

import taxes.Util.Logger
import taxes._

trait Exchanger extends Initializable {
  val id : String

  override def toString: String = id

  val sources : Seq[Source[Operation]]
}


object Exchangers {
  val allExchangers : List[Exchanger] =
    List[Exchanger](
        Binance
      , Bitfinex
      , Bittrex
      , CCEX
      , Changelly
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

  def readAllOperations(): List[Operation] = {
    var operations = List[Operation]()
    for (exchanger <- allExchangers) {
      Logger.trace("Reading data for %s.".format(exchanger))
      for (src <- exchanger.sources)
        operations ++= src.read()
    }
    return operations
  }
}


abstract case class UserFolderSource[+A](folderPath : String, extension : String) extends FolderSource[A](Paths.userInputFolder+"/"+folderPath, extension)
