package taxes

package object report {
  val decimalPlaces = 4.max(Config.config.decimalPlaces)

  def decimalPlaces(currency: Currency): Int =
    if(currency==Currency.euro || currency==Currency.usd)
      2
    else
    decimalPlaces

  def showBalance(balance: Double): Double =
    if(balance.abs < Config.config.epsilon) 0 else balance
}
