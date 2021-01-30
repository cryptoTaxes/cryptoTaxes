package taxes

import taxes.io.FileSystem
import taxes.util.parse.Parse

import spray.json._
import spray.json.JsonProtocol._

object Currency {
  private val conversions: Map[Currency, Currency] = {
    Parse.readKeysValue ( FileSystem.inConfigFolder("currencyNormalizations.txt")
                        , "Reading currency normalizations"
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

  val cardano: Currency = normalize("ADA")

  val euro: Currency = normalize("EURO")

  val usd: Currency = normalize("USD")

  val usdt: Currency = normalize("USDT")

  val ethereum: Currency = normalize("ETH")

  val ethereumClassic: Currency = normalize("ETC")

  val nxt: Currency = normalize("NXT")

  val litecoin: Currency = normalize("LTC")

  val dogecoin: Currency = normalize("DOGE")

  val vertcoin: Currency = normalize("VTC")

  val etc: Currency = normalize("ETC")

  val ripple: Currency = normalize("XRP")

  val stratis: Currency = normalize("STRAT")

  private val priorities: Map[Currency, Int] = {
    Parse.readKeysValue( FileSystem.inConfigFolder(Config.config.parityPrioritiesFile)
                       , "Reading currency priorities"
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

  private val names: Map[Currency, String] = {
    Parse.readKeysValue ( FileSystem.inConfigFolder("currencyNames.txt")
                        , "Reading currency names"
                        ).map{
      case (currency, name) => (normalize(currency.toUpperCase()), name)
    }
  }

  def nameOf(currency: Currency): Option[String] =
    names.get(normalize(currency))

  def fullName(currency: Currency): String = {
    val normalized = normalize(currency)
    nameOf(normalized) match {
      case None => normalized
      case Some(desc) =>
        if(desc==normalized)
          normalized
        else
          s"${normalize(currency)} - $desc"
    }
  }
}
