package taxes.PriceHistory

import taxes.Market.Market
import taxes.{Date, Price}

object CryptoUSDParity {
  def apply(market : Market, date : Date) : Price =
    CoinMarketCapPrice(market, date)
}
