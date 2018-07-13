package taxes

import taxes.util.parse.Parse

object Market extends Initializable {
  private val conversions : Map[Market, Market] = {
    def parsePair(t : (Market, Market)) = t match {
      case (market1, market2) => (market1.toUpperCase(), market2.toUpperCase())
    }
    Parse.readAssociations ( Paths.configFile("marketsNormalization.txt")
                           , "Reading market normalizations"
                           ).map(parsePair)
  }

  def normalize(coin0 : Market) : Market = {
    val coin = coin0.toUpperCase()
    conversions.get(coin) match {
      case None => coin
      case Some(normalized) => normalized
    }
  }

  val bitcoin : Market = normalize("BITCOIN")

  val euro : Market = normalize("EURO")

  val usd : Market = normalize("USD")

  val usdt : Market = normalize("USDT")

  val ethereum : Market = normalize("ETH")

  val nxt : Market = normalize("NXT")

  val litecoin : Market = normalize("LTC")

  val dogecoin : Market = normalize("DOGE")

  val vertcoin : Market = normalize("VTC")

  val etc : Market = normalize("ETC")

  val ripple : Market = normalize("XRP")


  private val priorities : Map[Market, Int] = {
    def parsePair(t : (Market, String)) = t match {
      case (market, str) => (normalize(market), str.toInt)
    }
    Parse.readAssociations( Paths.configFile("parityPriorities.txt")
                          , "Reading market priorities"
                          ).map(parsePair)
  }

  def priority(market : Market) : Int =
    priorities.getOrElse(market, 0)
}
