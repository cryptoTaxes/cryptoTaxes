package taxes

import java.io.PrintStream
import java.text.SimpleDateFormat

import taxes.Exchanger.Exchanger
import taxes.Market.Market
import taxes.Util.Logger


case class CSV() {

  private case class State(fileName : String, ps : java.io.PrintStream)

  private var optState : Option[State] = None

  private val header = List("Date","Sold","Amount", "Bought", "Amount", "Cost basis", "Proceeds", "Fee", "Exchanger")

  private val df = new SimpleDateFormat("yyyy-MM-dd")

  private val af = "%.8f"
  private val ff = "%.2f"

  private val sep = ";"

  case class Entry(date : Option[Date] = None, sold : Option[Market] = None, soldAmount : Option[Double] = None, bought : Option[Market] = None, boughtAmount : Option[Double] = None, costBasis : Option[Double] = None, sellValue : Option[Double] = None, fee : Option[Double] = None, exchanger: Exchanger) {

    private def doFormat[A](opt : Option[A], f : A => String) : String = opt match {
      case None => ""
      case Some(x) => f(x)
    }

    override def toString = {
      val xs = List[String](
                     doFormat[Date](date, df.format(_))
                   , doFormat[String](sold, "%s".format(_))
                   , doFormat[Double](soldAmount, af.format(_))
                   , doFormat[String](bought, "%s".format(_))
                   , doFormat[Double](boughtAmount, af.format(_))
                   , doFormat[Double](costBasis, ff.format(_))
                   , doFormat[Double](sellValue, ff.format(_))
                   , doFormat[Double](fee, ff.format(_))
                   , exchanger.toString
                   )
      xs.mkString(sep)
    }
  }

  def setOutputTo(fileName : String): Unit = {
    val newState = State(fileName, new PrintStream(fileName))

    optState match {
      case None     => ;
      case Some(st) => st.ps.close()
    }

    optState = Some(newState)
  }

  def close() : Unit = {
    optState match {
      case None     => Logger.fatal("CSV.close: stream is already closed")
      case Some(st) => st.ps.close()
    }
  }

  def println(str : String) : Unit = {
    optState match {
      case None     => Logger.fatal("CSV.println: stream is currently closed")
      case Some(st) => st.ps.println(str)
    }
  }

  def println : Unit = {
    println("")
  }

  def println(x : Any) : Unit = {
    println(x.toString)
  }

  def printlnHeader =
    println(header.mkString(sep))
}
