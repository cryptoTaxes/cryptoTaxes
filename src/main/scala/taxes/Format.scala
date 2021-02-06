package taxes

import taxes.date.LocalDateTimeDiff

import java.math.RoundingMode
import java.text.DecimalFormat


object Format {
  val df = java.time.format.DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")

  val shortDf = java.time.format.DateTimeFormatter.ofPattern("yyyy-MM-dd")

  val decimalSep: Char =
    java.text.NumberFormat.getInstance().asInstanceOf[DecimalFormat].getDecimalFormatSymbols.getDecimalSeparator

  def trimZeros(_str: String): String = {
    var str = _str.reverse
    if(_str.contains(decimalSep)) {
      str = str.dropWhile(_ == '0')
      if(str.nonEmpty && str.head == decimalSep)
        str = str.tail
    }
    return str.reverse
  }

  def formatDecimalOld(x: Double, decimals: Int = Config.config.decimalPlaces): String = {
    if(x.isInfinite)
      return "∞"
    else if(x.isNaN)
      return "NaN"
    val xAbs = x.abs
    var fmt =
      if(xAbs <= 0.00000009)
        "0.000000000"
      else if(xAbs <= 0.0000009)
        "0.00000000"
      else if(xAbs <= 0.000009)
        "0.0000000"
      else if(xAbs <= 0.00009)
        "0.000000"
      else if(xAbs <= 0.0009)
        "0.00000"
      else if(xAbs <= 0.009)
        "0.0000"
      else if(xAbs <= 0.09)
        "0.000"
      else
        "0.00"

    while(fmt.length - 2 < decimals)
      fmt = fmt + '0'

    val df = new DecimalFormat(fmt)
    df.setRoundingMode(RoundingMode.DOWN)
    return trimZeros(df.format(x))
  }

  def formatDecimal(x: Double, tailDigits: Int = Config.config.decimalPlaces): String = {
    if(x.isInfinite)
      return "∞"
    else if(x.isNaN)
      return "NaN"
    val xAbs = x.abs
    var fmt =
      if(xAbs <= 0.00000009)
        "0.00000000"
      else if(xAbs <= 0.0000009)
        "0.0000000"
      else if(xAbs <= 0.000009)
        "0.000000"
      else if(xAbs <= 0.00009)
        "0.00000"
      else if(xAbs <= 0.0009)
        "0.0000"
      else if(xAbs <= 0.009)
        "0.000"
      else if(xAbs <= 0.09)
        "0.00"
      else if(xAbs <= 0.9)
        "0.0"
      else
        "0.0"

    val maxDecimals = 8

    var added = 0
    while(fmt.length - 2 < maxDecimals && added < tailDigits-1) {
      fmt = fmt + '0'
      added += 1
    }

    val df = new DecimalFormat(fmt)
    df.setRoundingMode(RoundingMode.HALF_DOWN)
    return trimZeros(df.format(x))
  }

  def asCurrency(amount: Double, currencyUnit: Currency, decimals: Int = Config.config.decimalPlaces): String =
    Format.formatDecimal(amount, decimals) + " " + currencyUnit

  def asTimeDiff(lDTD: LocalDateTimeDiff, toShowItems: Int = 2) = {
    val longLabels = Array("years", "month", "day", "hour", "minute", "second")
    val labels = Array("Y", "M", "D", "h", "m", "s")
    val values = Array(lDTD.years, lDTD.months, lDTD.days, lDTD.hours, lDTD.minutes, lDTD.seconds)

    var items = 0
    var str = ""
    var i = 0
    while(items < toShowItems && i < values.length) {
      if(values(i)>0) {
        items += 1
        if(items > 1)
          str += "/"
        str += s"${values(i)}${labels(i)}"
      }
      i += 1
    }

    if(str.isEmpty)
      str = s"0${labels.last}"
    str
  }
}
