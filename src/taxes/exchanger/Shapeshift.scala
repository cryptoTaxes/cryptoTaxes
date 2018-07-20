package taxes.exchanger

import taxes._
import taxes.date._
import taxes.util._
import taxes.util.parse.{JsObjectAST, Parse}


object Shapeshift extends Exchanger {
  override val id: String = "Shapeshift"

  override val sources = Seq(
    new UserFolderSource[Operation]("shapeshift", ".json") {
      def fileSource(fileName : String) = new FileSource[Operation](fileName) {
        override def read(): Seq[Operation] =
          readFile(fileName)
      }
    }
  )

  def readFile(fileName : String) : List[Exchange] = {
    val prefix0 = "https://shapeshift.io/#/status/"
    val prefix1 = "https://shapeshift.io/txStat/"

    var lnNumber = 0
    val f = new java.io.File(fileName)
    val sc = new java.util.Scanner(f)

    def nextLine(): String = {
      lnNumber += 1
      Parse.trimSpaces(sc.nextLine())
    }

    var exchanges = List[Exchange]()
    while (sc.hasNextLine) {
      val ln0 = nextLine()
      if(ln0.nonEmpty) {
        if(!ln0.startsWith(prefix0))
          Logger.warning("%s. %s Line %d: \"%s\" should start with %s.".format(id, FileSystem.pathFromData(fileName), lnNumber, ln0, prefix0))
        else {
          val orderId = ln0.drop(prefix0.length)
          val desc = "Order: " + orderId

          val ln1 = nextLine()
          if (!ln1.startsWith(prefix1))
            Logger.warning("%s. %s Line %d: \"%s\" should start with %s.".format(id, FileSystem.pathFromData(fileName), lnNumber, ln1, prefix1))
          else {
            val inAddress = ln1.drop(prefix1.length)

            val ln2 = nextLine()
            val json = JsObjectAST.fromString(ln2)

            val addr = json.getString("address")
            if (addr != inAddress)
              Logger.warning("%s. %s Line %d: Input address %s should be %s.".format(id, FileSystem.pathFromData(fileName), lnNumber, addr, inAddress))
            else if(json.getString("status") != "complete")
              Logger.warning("%s. %s Line %d: Status should be complete.".format(id, FileSystem.pathFromData(fileName), lnNumber))
            else {
              val fromAmount = json.getDouble("incomingCoin")
              val fromMarket = Market.normalize(json.getString("incomingType"))
              val address1 = json.getString("address")

              val toAmount = json.getDouble("outgoingCoin")
              val toMarket = Market.normalize(json.getString("outgoingType"))

              val ln3 = nextLine()
              val txid = ln3

              val txInfo = TransactionsCache.lookup(fromMarket, txid, address1)

              val date = txInfo.localDate

              val exchange =
                Exchange(
                  date = date
                  , id = orderId
                  , fromAmount = fromAmount, fromMarket = fromMarket
                  , toAmount = toAmount, toMarket = toMarket
                  , fees = List(FeePair(txInfo.fee, fromMarket))
                  , exchanger = Shapeshift
                  , description = desc
                )

              exchanges ::= exchange
            }
          }
        }
      }
    }
    sc.close()
    return exchanges.sortBy(_.date)
  }
}


