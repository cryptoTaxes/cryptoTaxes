package taxes.priceHistory

import taxes._
import taxes.date._

object CryptoUSDParity {
  def apply(market: Market, date: LocalDateTime): Price =
    CoinMarketCapPrice(market, date)
}
