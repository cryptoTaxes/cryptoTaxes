package taxes

import java.math.RoundingMode
import java.text.{DecimalFormat, SimpleDateFormat}

import taxes.Market.Market

object Format {
  val df = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")

  val shortDf = new SimpleDateFormat("yyyy-MM-dd")

  val decimalSep : Char =
    java.text.NumberFormat.getInstance().asInstanceOf[DecimalFormat].getDecimalFormatSymbols.getDecimalSeparator

  def trimZeros(str0 : String) : String = {
    var str = str0.reverse
    if(str0.contains(decimalSep)) {
      str = str.dropWhile(_ == '0')
      if (str.nonEmpty && str.head == decimalSep)
        str = str.tail
    }
    return str.reverse
  }

  def formatDecimal(x : Double, decimals : Int = Config.config.decimalPlaces) : String = {
    if(x.isInfinite)
      return "∞"
    else if(x.isNaN)
      return "NaN"
    val xAbs = x.abs
    var fmt =
      if (xAbs <= 0.00000009)
        "0.000000000"
      else if (xAbs <= 0.0000009)
        "0.00000000"
      else if (xAbs <= 0.000009)
        "0.0000000"
      else if (xAbs <= 0.00009)
        "0.000000"
      else if (xAbs <= 0.0009)
        "0.00000"
      else if (xAbs <= 0.009)
        "0.0000"
      else if (xAbs <= 0.09)
        "0.000"
      else
        "0.00"

    while(fmt.length - 2 < decimals)
      fmt = fmt + '0'

    val df = new DecimalFormat(fmt)
    df.setRoundingMode(RoundingMode.DOWN)
    return trimZeros(df.format(x))
  }

  def asMarket(amount : Double, marketUnit : Market, decimals : Int = Config.config.decimalPlaces) : String =
    Format.formatDecimal(amount, decimals) + " " + marketUnit
}
