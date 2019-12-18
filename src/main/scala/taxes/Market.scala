package taxes

import taxes.io.FileSystem
import taxes.util.parse.Parse

import spray.json._
import spray.json.JsonProtocol._

object Market {
  private val conversions: Map[Market, Market] = {
    Parse.readKeysValue ( FileSystem.readConfigFile("marketsNormalization.txt")
                        , "Reading market normalizations."
                        ).map{
      case (market1, market2) => (market1.toUpperCase(), market2.toUpperCase())
    }
  }

  def normalize(coin0: Market): Market = {
    val coin = coin0.toUpperCase()
    conversions.get(coin) match {
      case None => coin
      case Some(normalized) => normalized
    }
  }

  val bitcoin: Market = normalize("BITCOIN")

  val euro: Market = normalize("EURO")

  val usd: Market = normalize("USD")

  val usdt: Market = normalize("USDT")

  val ethereum: Market = normalize("ETH")

  val nxt: Market = normalize("NXT")

  val litecoin: Market = normalize("LTC")

  val dogecoin: Market = normalize("DOGE")

  val vertcoin: Market = normalize("VTC")

  val etc: Market = normalize("ETC")

  val ripple: Market = normalize("XRP")


  private val priorities: Map[Market, Int] = {
    Parse.readKeysValue( FileSystem.readConfigFile(Config.config.parityPrioritiesFile)
                       , "Reading market priorities."
                       ).map{
      case (market, str) => (normalize(market), Parse.asInt(str))
    }
  }

  def priority(market: Market): Int =
    priorities.getOrElse(market, 0)

  def saveToFile(path: String): Unit = {
    val json = JsObject(
      "conversions" -> JsArray(conversions.toVector.sortBy(_._1).map(_.toJson))
      , "priorities" -> JsArray(priorities.toVector.sortBy(- _._2).map(_.toJson))
    )

    FileSystem.withPrintStream(path) {
      _.println(json.prettyPrint)
    }
  }
}
