package taxes.Exchanger

import taxes.Util.Logger
import taxes._

trait Exchanger extends Initializable {
  val id : String
  val folder : String
  def readFile(fileName : String) : List[Operation]

  override def toString: String = id
}


object Exchangers {
  val allExchangers : List[Exchanger] =
    List[Exchanger](
        Bittrex
      , CCEX
      , Changelly
      , General
      , GDAX
      , HitBTC
      , Kraken
      , LocalBTC
      , Poloniex
      , PoloniexBorrowing
      , Shapeshift
      , RippleTrade
      , Yobit
      )

  def readAllOperations(user: String): List[Operation] = {
    var operations = List[Operation]()
    for (exchanger <- allExchangers) {
      val files = Paths.findFilesAt(Paths.userInputFolder + "/" + exchanger.folder)
      if (files != null)
        for (file <- files) {
          val fileName = file.getAbsolutePath
          Logger.trace("Reading %s trade history from %s.".format(exchanger.id, fileName))
          operations ++= exchanger.readFile(fileName)
        }
    }
    return operations
  }
}

