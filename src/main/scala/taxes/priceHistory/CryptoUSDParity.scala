package taxes.priceHistory

import taxes._
import taxes.date._

object CryptoUSDParity {
  def apply(currency: Currency, date: LocalDateTime): Price =
    CryptoPriceProvider.priceInUSD(currency, date)
}
