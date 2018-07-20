package taxes.exchanger

import taxes._
import taxes.date._
import taxes.util._
import taxes.util.parse._

import scala.io.Source


object RippleTrade extends Exchanger {
  override val id: String = "XRPTrade"

  override val sources = Seq(
    new UserFolderSource[Operation]("xrptrade", ".json") {
      def fileSource(fileName: String) = new FileSource[Operation](fileName) {
        override def read(): Seq[Operation] =
          readFile(fileName)
      }
    }
  )

  private case class Entry(hash : String, amount : Double, currency : Market, date : LocalDateTime)

  private def readFile(fileName : String) : List[Exchange] = {
    val contents = FileSystem.withSource(fileName){ src =>
      src.dropWhile(_ != '{').mkString // skip till proper start of json
    }

    val json = JsObjectAST.fromString(contents)

    val changes = json.getVector("balance_changes")

    val entries = for(change <- changes; changeJson = JsObjectAST.fromJsValue(change))
      yield
        Entry(
          date = LocalDateTime.parse(changeJson.getString("executed_time"), "yyyy-MM-dd'T'HH:mm:ssX")
          , hash = changeJson.getString("tx_hash")
          , amount = Parse.asDouble(changeJson.getString("amount_change"))
          , currency = changeJson.getString("currency")
        )

    var exchanges = List[Exchange]()

    val hashes = entries.map(_.hash).toSet

    for(hash <- hashes) {
      val optXRP = entries.find(entry => entry.hash == hash && entry.currency == "XRP")
      optXRP match {
        case None =>
          Logger.fatal("RippleTrade.readFile %s: could not find XRP value for hash %s".format(FileSystem.pathFromData(fileName), hash))
        case Some(entryXRP) => {
          val optBTC = entries.find(entry => entry.hash == hash && entry.currency == "BTC")
          optBTC match {
            case None =>
              Logger.fatal("RippleTrade.readFile %s: could not find BTC value for hash %s".format(FileSystem.pathFromData(fileName), hash))
            case Some(entryBTC) => {
              val desc = "Order: " + hash
              val exchange =
                if (entryXRP.amount < 0)
                  Exchange(
                    date = entryXRP.date
                    , id = hash
                    , fromAmount = entryXRP.amount.abs, fromMarket = Market.ripple
                    , toAmount = entryBTC.amount.abs, toMarket = Market.bitcoin
                    , fees = List(FeePair(0.012, Market.ripple))
                    , exchanger = RippleTrade
                    , description = desc
                  )
                else
                  Exchange(
                    date = entryXRP.date
                    , id = hash
                    , fromAmount = entryBTC.amount.abs, fromMarket = Market.bitcoin
                    , toAmount = entryXRP.amount.abs, toMarket = Market.ripple
                    , fees = List(FeePair(0.012, Market.ripple))
                    , exchanger = RippleTrade
                    , description = desc
                  )
              exchanges ::= exchange
            }
          }
        }
      }
    }
    return exchanges.sortBy(_.date)
  }
}
