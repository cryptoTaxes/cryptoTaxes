package taxes.priceHistory

import taxes.date.LocalDateTime
import taxes.{Config, Currency, Price, PriceProvider}

trait CryptoPriceProvider {
  def toString: String

  def downloadPrices(): Unit

  def priceInUSD(currency: Currency, date: LocalDateTime): Price
}

object CryptoPriceProvider extends CryptoPriceProvider {
  private lazy val cryptoPriceProvider: CryptoPriceProvider =
    Config.config.priceProvider match {
      case PriceProvider.CoinGecko => CoinGeckoPriceProvider
      case PriceProvider.CoinMarketCap => CoinMarketCapPriceProvider
    }

  override def toString: String =
    cryptoPriceProvider.toString

  def downloadPrices(): Unit =
    cryptoPriceProvider.downloadPrices()

  // returns price in USD for a currency at a given date
  def priceInUSD(currency: Currency, date: LocalDateTime): Price =
    cryptoPriceProvider.priceInUSD(currency, date)
}
