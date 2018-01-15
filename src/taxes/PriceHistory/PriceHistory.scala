package taxes.PriceHistory

import taxes._

trait PriceHistory {
  def apply(date : Date) : Price
}






