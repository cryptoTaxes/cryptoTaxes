package taxes

import taxes.io.FileSystem
import taxes.util.parse.Parse

import spray.json._
import spray.json.JsonProtocol._

object Currency {
  private val conversions: Map[Currency, Currency] = {
    Parse.readKeysValue ( FileSystem.readConfigFile("currencyNormalizations.txt")
                        , "Reading currencies normalizations."
                        ).map{
      case (currency1, currency2) => (currency1.toUpperCase(), currency2.toUpperCase())
    }
  }

  def normalize(_currency: Currency): Currency = {
    val currency = _currency.toUpperCase()
    conversions.get(currency) match {
      case None => currency
      case Some(normalized) => normalized
    }
  }

  val bitcoin: Currency = normalize("BITCOIN")

  val euro: Currency = normalize("EURO")

  val usd: Currency = normalize("USD")

  val usdt: Currency = normalize("USDT")

  val ethereum: Currency = normalize("ETH")

  val nxt: Currency = normalize("NXT")

  val litecoin: Currency = normalize("LTC")

  val dogecoin: Currency = normalize("DOGE")

  val vertcoin: Currency = normalize("VTC")

  val etc: Currency = normalize("ETC")

  val ripple: Currency = normalize("XRP")


  private val priorities: Map[Currency, Int] = {
    Parse.readKeysValue( FileSystem.readConfigFile(Config.config.parityPrioritiesFile)
                       , "Reading currencies priorities."
                       ).map{
      case (currency, str) => (normalize(currency), Parse.asInt(str))
    }
  }

  def priority(currency: Currency): Int =
    priorities.getOrElse(currency, 0)

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
