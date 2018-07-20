package taxes

import java.text.{DecimalFormat, NumberFormat}

import taxes.date._
import taxes.priceHistory.{CryptoUSDParity, EuroUSDParity}


object PriceUtils {
  def priceToUSDs(price : Price, priceUnit: Market, date : LocalDateTime) : Price = {
    if(priceUnit == Market.usd)
      price
    else if(priceUnit == Market.euro)
      EuroUSDParity.euro2USD(price, date)
    else
      price * CryptoUSDParity(priceUnit, date)
  }

  def priceInUSDs(market: Market, date : LocalDateTime) : Price =
    priceToUSDs(1, market, date)


  def priceToEuros(price : Price, priceUnit: Market, date : LocalDateTime) : Price = {
    if(priceUnit == Market.euro)
      price
    else
      price * EuroUSDParity.USD2Euro(priceInUSDs(priceUnit, date), date)
  }

  def priceInEuros(market: Market, date : LocalDateTime) : Price =
    priceToEuros(1, market, date)
}


trait BaseCoin {
  val market : Market

  val format : NumberFormat

  def toBaseCoin(price : Price, priceUnit: Market, date : LocalDateTime) : Price

  def priceInBaseCoin(market: Market, date : LocalDateTime) : Price =
    toBaseCoin(1, market, date)
}


object BTCBaseCoin extends BaseCoin {
  lazy val market = Market.bitcoin

  lazy val format = new DecimalFormat("0.########")

  private def USD2BTC(price : Price, date : LocalDateTime) : Price =
    price / CryptoUSDParity(Market.bitcoin, date)

  def toBaseCoin(price : Price, priceUnit: Market, date : LocalDateTime) : Price =
    if(priceUnit == Market.bitcoin)
      price
    else if(priceUnit == Market.euro)
      EuroUSDParity.euro2USD(price, date) / PriceUtils.priceInUSDs(Market.bitcoin, date)
    else
      price * USD2BTC(PriceUtils.priceInUSDs(priceUnit, date), date)
}


object EuroBaseCoin extends BaseCoin {
  lazy val market = Market.euro

  lazy val format = new DecimalFormat("0.#####")

  def toBaseCoin(price : Price, priceUnit: Market, date : LocalDateTime) : Price =
    PriceUtils.priceToEuros(price, priceUnit, date)
}

object USDBaseCoin extends BaseCoin {
  lazy val market = Market.usd

  lazy val format = new DecimalFormat("0.#####")

  def toBaseCoin(price : Price, priceUnit: Market, date : LocalDateTime) : Price =
    PriceUtils.priceToUSDs(price, priceUnit, date)
}