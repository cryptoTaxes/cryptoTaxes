package taxes.Exchanger

import taxes._

trait Exchanger extends Initializable {
  val id : String

  override def toString: String = id

  val sources : Seq[Source[Operation]]
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
      , Shapeshift
      , RippleTrade
      , Yobit
      )

  def readAllOperations(user: String): List[Operation] = {
    var operations = List[Operation]()
    for (exchanger <- allExchangers)
      for (src <- exchanger.sources)
        operations ++= src.read()
    return operations
  }
}


abstract case class UserFolderSource[+A](folderPath : String) extends FolderSource[A](Paths.userInputFolder+"/"+folderPath)
