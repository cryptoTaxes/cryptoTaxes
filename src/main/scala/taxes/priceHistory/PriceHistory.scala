package taxes.priceHistory

import taxes._
import taxes.date._


trait PriceHistory {
  def apply(date: LocalDateTime): Price
}






