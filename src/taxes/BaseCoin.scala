package taxes

import java.text.{DecimalFormat, NumberFormat}
import taxes.Market.Market
import taxes.PriceHistory.{CryptoUSDParity, EuroUSDParity}

object PriceUtils {
  def priceToUDSs(price : Price, priceUnit: Market, date : Date) : Price = {
    if(priceUnit == Market.usd)
      price
    else if(priceUnit == Market.euro)
      EuroUSDParity.euro2USD(price, date)
    else
      price * CryptoUSDParity(priceUnit, date)
  }

  def priceInUSDs(market: Market, date : Date) : Price =
    priceToUDSs(1, market, date)


  def priceToEuros(price : Price, priceUnit: Market, date : Date) : Price = {
    if(priceUnit == Market.euro)
      price
    else
      price * EuroUSDParity.USD2Euro(priceInUSDs(priceUnit, date), date)
  }

  def priceInEuros(market: Market, date : Date) : Price =
    priceToEuros(1, market, date)
}


trait BaseCoin {
  val market : Market

  val format : NumberFormat

  def toBaseCoin(price : Price, priceUnit: Market, date : Date) : Price

  def priceInBaseCoin(market: Market, date : Date) : Price =
    toBaseCoin(1, market, date)
}


object BTCBaseCoin extends BaseCoin {
  val market = Market.bitcoin

  val format = new DecimalFormat("0.########")

  private def USD2BTC(price : Price, date : Date) : Price =
    price / CryptoUSDParity(Market.bitcoin, date)

  def toBaseCoin(price : Price, priceUnit: Market, date : Date) : Price =
    if(priceUnit == Market.bitcoin)
      price
    else if(priceUnit == Market.euro)
      EuroUSDParity.euro2USD(price, date) / PriceUtils.priceInUSDs(Market.bitcoin, date)
    else
      price * USD2BTC(PriceUtils.priceInUSDs(priceUnit, date), date)
}


object EuroBaseCoin extends BaseCoin {
  val market = Market.euro

  val format = new DecimalFormat("0.#####")

  def toBaseCoin(price : Price, priceUnit: Market, date : Date) : Price =
    PriceUtils.priceToEuros(price, priceUnit, date)
}

object USDBaseCoin extends BaseCoin {
  val market = Market.usd

  val format = new DecimalFormat("0.#####")

  def toBaseCoin(price : Price, priceUnit: Market, date : Date) : Price =
    PriceUtils.priceToUDSs(price, priceUnit, date)
}