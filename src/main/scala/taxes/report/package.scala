package taxes

package object report {
  val decimalPlaces = 4.max(Config.config.decimalPlaces)

  def showBalance(balance: Double): Double =
    if(balance.abs < Config.config.epsilon) 0 else balance
}
