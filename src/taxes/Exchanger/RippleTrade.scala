package taxes.Exchanger

import taxes.Market.Market
import taxes.Util.Logger
import taxes.Util.Parse._
import taxes._

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

  private case class Entry(hash : String, amount : Double, currency : Market, date : Date)

  private def readFile(fileName : String) : List[Exchange] = {
    val src = Source.fromFile(fileName)
    val contents = src.dropWhile(_ != '{').mkString // skip till proper start of json
    src.close()

    val json = AdvancedJSONParser(contents)

    val changes = json.getList("balance_changes")

    val entries = for(change <- changes)
      yield
        Entry(
            date = Date.fromString(change[String]("executed_time"), "yyyy-MM-dd'T'HH:mm:ssX")
          , hash = change[String]("tx_hash")
          , amount = Parse.asDouble(change[String]("amount_change"))
          , currency = change[String]("currency")
        )

    var exchanges = List[Exchange]()

    val hashes = entries.map(_.hash).toSet

    for(hash <- hashes) {
      val optXRP = entries.find(entry => entry.hash == hash && entry.currency == "XRP")
      optXRP match {
        case None =>
          Logger.fatal("RippleTrade.readFile %s: could not find XRP value for hash %s".format(Paths.pathFromData(fileName), hash))
        case Some(entryXRP) => {
          val optBTC = entries.find(entry => entry.hash == hash && entry.currency == "BTC")
          optBTC match {
            case None =>
              Logger.fatal("RippleTrade.readFile %s: could not find BTC value for hash %s".format(Paths.pathFromData(fileName), hash))
            case Some(entryBTC) => {
              val desc = RippleTrade + " " + hash
              val exchange =
                if (entryXRP.amount < 0)
                  Exchange(
                    date = entryXRP.date
                    , id = hash
                    , fromAmount = entryXRP.amount.abs, fromMarket = Market.ripple
                    , toAmount = entryBTC.amount.abs, toMarket = Market.bitcoin
                    , fee = 0, feeMarket = Market.ripple
                    , exchanger = RippleTrade
                    , description = desc
                  )
                else
                  Exchange(
                    date = entryXRP.date
                    , id = hash
                    , fromAmount = entryBTC.amount.abs, fromMarket = Market.bitcoin
                    , toAmount = entryXRP.amount.abs, toMarket = Market.ripple
                    , fee = 0, feeMarket = Market.ripple
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
