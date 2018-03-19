package taxes

import java.text.SimpleDateFormat

import taxes.Market.Market

case class CSV(fileName : String) extends java.io.PrintStream(fileName) {

  private val header = List("Date","Sold","Amount", "Bought", "Amount", "Cost basis", "Sell Value", "Fee")

  private val df = new SimpleDateFormat("yyyy-MM-dd")

  private val af = "%.8f"
  private val ff = "%.2f"

  private val sep = ";"

  case class Entry(date : Date, sold : Market, soldAmount : Double, bought : Market, boughtAmount : Double, costBasis : Double, sellValue : Double, fee : Double) {
    override def toString = {
      val xs = List( df format date
                   , "%s" format sold
                   , af format soldAmount
                   , "%s" format bought
                   , af format boughtAmount
                   , ff format costBasis
                   , ff format sellValue
                   , ff format fee
                   )
      xs.mkString(sep)
    }
  }

  def printlnHeader =
    println(header.mkString(sep))
}
