package taxes

import taxes.date._
import taxes.priceHistory.{CryptoUSDParity, EuroUSDParity}

import spray.json._

import java.text.{DecimalFormat, NumberFormat}

object PriceUtils {
  def priceToUSDs(price: Price, priceUnit: Currency, date: LocalDateTime): Price = {
    if(priceUnit == Currency.usd)
      price
    else if(priceUnit == Currency.euro)
      EuroUSDParity.euro2USD(price, date)
    else
      price * CryptoUSDParity(priceUnit, date)
  }

  def priceInUSDs(currency: Currency, date: LocalDateTime): Price =
    priceToUSDs(1, currency, date)


  def priceToEuros(price: Price, priceUnit: Currency, date: LocalDateTime): Price = {
    if(priceUnit == Currency.euro)
      price
    else
      price * EuroUSDParity.USD2Euro(priceInUSDs(priceUnit, date), date)
  }

  def priceInEuros(currency: Currency, date: LocalDateTime): Price =
    priceToEuros(1, currency, date)
}


object BaseCurrency {
  implicit object baseCurrencyJson extends JsonFormat[BaseCurrency] {
    def write(baseCurrency: BaseCurrency) = {
      JsString(baseCurrency.currency)
    }

    def read(value: JsValue) =
      try {
        value match {
          case JsString(currency) => currency match {
            case BTCBaseCurrency.`currency` => BTCBaseCurrency
            case EuroBaseCurrency.`currency` => EuroBaseCurrency
            case USDBaseCurrency.`currency` => USDBaseCurrency
          }
        }
      } catch {
        case _ => deserializationError("BaseCurrency expected")
      }
  }}

sealed trait BaseCurrency {
  val currency: Currency

  val format: NumberFormat

  protected def toThisBaseCurrency(price: Price, priceUnit: Currency, date: LocalDateTime): Price

  // expressed in this base currency
  def priceOf(currency: Currency, date: LocalDateTime): Price =
    toThisBaseCurrency(1, currency, date)
}


object BTCBaseCurrency extends BaseCurrency {
  lazy val currency = Currency.bitcoin

  lazy val format = new DecimalFormat("0.########")

  private def USD2BTC(price: Price, date: LocalDateTime): Price =
    price / CryptoUSDParity(Currency.bitcoin, date)

  def toThisBaseCurrency(price: Price, priceUnit: Currency, date: LocalDateTime): Price =
    if(priceUnit == Currency.bitcoin)
      price
    else if(priceUnit == Currency.euro)
      EuroUSDParity.euro2USD(price, date) / PriceUtils.priceInUSDs(Currency.bitcoin, date)
    else
      price * USD2BTC(PriceUtils.priceInUSDs(priceUnit, date), date)
}


object EuroBaseCurrency extends BaseCurrency {
  lazy val currency = Currency.euro

  lazy val format = new DecimalFormat("0.#####")

  def toThisBaseCurrency(price: Price, priceUnit: Currency, date: LocalDateTime): Price =
    PriceUtils.priceToEuros(price, priceUnit, date)
}

object USDBaseCurrency extends BaseCurrency {
  lazy val currency = Currency.usd

  lazy val format = new DecimalFormat("0.#####")

  def toThisBaseCurrency(price: Price, priceUnit: Currency, date: LocalDateTime): Price =
    PriceUtils.priceToUSDs(price, priceUnit, date)
}