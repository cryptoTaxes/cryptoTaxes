package taxes

import java.text.SimpleDateFormat

import taxes.date._
import taxes.exchanger.Exchanger
import taxes.io.FileSystem
import taxes.util.Logger


case class CSV() {
  private case class State(fileName: String, ps: FileSystem.PrintStream)

  private var optState: Option[State] = None

  private val header = List("Date", "Sold", "Amount", "Bought", "Amount", "Cost basis", "Proceeds", "Fee", "Exchanger")

  private val df = new SimpleDateFormat("yyyy-MM-dd")

  private val af = "%.8f"
  private val ff = "%.2f"

  private val sep = ";"

  case class Entry(date: Option[LocalDateTime] = None, sold: Option[Currency] = None, soldAmount: Option[Double] = None, bought: Option[Currency] = None, boughtAmount: Option[Double] = None, costBasis: Option[Double] = None, sellValue: Option[Double] = None, fee: Option[Double] = None, exchanger: Exchanger) {

    private def doFormat[A](opt: Option[A], f: A => String): String = opt match {
      case None => ""
      case Some(x) => f(x)
    }

    override def toString = {
      val xs = List[String](
                     doFormat[LocalDateTime](date, df.format(_))
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

  def setOutputTo(fileName: String): Unit = {
    val newState = State(fileName, FileSystem.PrintStream(fileName))

    optState match {
      case None     => ;
      case Some(st) => st.ps.close()
    }

    optState = Some(newState)
  }

  def close(): Unit = {
    optState match {
      case None     => Logger.fatal("CSV.close: stream is already closed")
      case Some(st) => st.ps.close()
    }
  }

  def println(str: String): Unit = {
    optState match {
      case None     => Logger.fatal("CSV.println: stream is currently closed")
      case Some(st) => st.ps.println(str)
    }
  }

  def println(): Unit =
    println("")

  def println(x: Any): Unit =
    println(x.toString)

  def printlnHeader(): Unit =
    println(header.mkString(sep))
}
