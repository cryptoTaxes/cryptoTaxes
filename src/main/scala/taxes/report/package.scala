package taxes

import taxes.date._

package object report {
  val decimalPlaces = 4.max(Config.config.decimalPlaces)

  protected def decimalPlaces(currency: Currency): Int =
    if(currency==Currency.euro || currency==Currency.usd)
      2
    else
      decimalPlaces

  object Format {
    private def smallClass(cls: String, small: Boolean): String =
      if(small) cls+" small2" else cls

    def asAmount(n: Double, currency: Currency, signed: Boolean = false, colored: Boolean = true) = {
      <span class={if(!colored) "" else if(n<0) "darkRed" else "darkBlue"}>
       {(if(signed && n>0) "+" else "") + taxes.Format.formatDecimal(n, report.decimalPlaces(currency))}
      </span>
    }

    def asCurrency(amount: Double, currency: Currency, decimals: Int, small: Boolean): HTML =
      <span class='noLineBreak'>
        {taxes.Format.formatDecimal(amount, decimals)}<span class={smallClass("currency", small)}
      >&#x2006;{currency}</span>
      </span>

    def asCurrency(amount: Double, currency: Currency, signed: Boolean = false, colored: Boolean = true): HTML = {
      <span class={if(!colored) "" else if(amount<0) "darkRed" else "darkBlue"}>
        {asCurrency(amount, currency, decimals = report.decimalPlaces(currency), small = true)}
      </span>
    }

    def asRate(price: Price, currency1: Currency, currency2: Currency, decimals: Int = Config.config.decimalPlaces, small: Boolean = true): HTML =
      <span class='noLineBreak'>
        {taxes.Format.formatDecimal(price, decimals)}
        <span class={smallClass("currency", small)}>{currency1}</span
        ><span class={smallClass("", small)}>&#x2006;/&#x2006;</span
      ><span class={smallClass("currency", small)}>{currency2}</span>
      </span>

    def asRate(price: Price, currency1: Currency, currency2: Currency): HTML =
      asRate(price, currency1, currency2, decimals = report.decimalPlaces(currency1), small = true)

    def asDate(date: LocalDateTime) =
      <span class='paddingL small1'>
        {taxes.Format.df.format(date)}
      </span>

    def asTimeDiff(to: LocalDateTime, from: LocalDateTime) =
      <span class={if(to.atLeast1YearFrom(from)) "" else "darkRed"}>
        {taxes.Format.asTimeDiff(to.difference(from))}
      </span>
  }
}
