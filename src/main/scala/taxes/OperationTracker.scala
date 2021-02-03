package taxes

import taxes.date.LocalDateTime
import taxes.exchanger.{Exchanger, General}
import taxes.io.FileSystem


object OperationTracker {
  case class CSVEntry( date: LocalDateTime
                       , exchanger: Exchanger
                       , description: String
                       , costBasis: Double
                       , proceeds: Double
                       , fee: Double
                     )
  private val emptyEntry = OperationTracker.CSVEntry(LocalDateTime(), new General(""), "", 0, 0, 0)
}

case class OperationTracker() extends Iterable[(Int,OperationTracker.CSVEntry)] {
  private val m = scala.collection.mutable.Map[Int, OperationTracker.CSVEntry]()

  override def iterator: Iterator[(Int,OperationTracker.CSVEntry)] =
    m.iterator.toList.sortBy(_._1).toIterator

  def recordCostBasis(operationNumber: Int, amount: Double): Unit = {
    val entry = m.getOrElse(operationNumber, OperationTracker.emptyEntry)
    m(operationNumber) = entry.copy(costBasis = entry.costBasis + amount)
  }

  def recordProceeds(operationNumber: Int, amount: Double): Unit = {
    val entry = m.getOrElse(operationNumber, OperationTracker.emptyEntry)
    m(operationNumber) = entry.copy(proceeds = entry.proceeds + amount)
  }

  def recordFee(operationNumber: Int, amount: Double): Unit = {
    val entry = m.getOrElse(operationNumber, OperationTracker.emptyEntry)
    m(operationNumber) = entry.copy(fee = entry.fee + amount)
  }

  def setDate(operationNumber: Int, date: LocalDateTime): Unit = {
    val entry = m.getOrElse(operationNumber, OperationTracker.emptyEntry)
    m(operationNumber) = entry.copy(date = date)
  }

  def setDescription(operationNumber: Int, description: String): Unit = {
    val entry = m.getOrElse(operationNumber, OperationTracker.emptyEntry)
    m(operationNumber) = entry.copy(description = description)
  }

  def setExchanger(operationNumber: Int, exchanger: Exchanger): Unit = {
    val entry = m.getOrElse(operationNumber, OperationTracker.emptyEntry)
    m(operationNumber) = entry.copy(exchanger = exchanger)
  }

  def clear(): Unit =
    m.clear()

  def printToCSVFile(fileName: String, year: Int, baseCurrency: Currency): Unit = {
    FileSystem.withPrintStream(fileName) { ps =>

      ps.println()
      ps.println(s"${Accounting.toString(Config.config.accountingMethod)} $year")
      ps.println("")

      val sep = ";"
      val header = List("Order", "Date", "Exchanger", "Description"
        , s"Cost basis/Loss ($baseCurrency)", s"Proceeds/Gain ($baseCurrency)", s"Fee ($baseCurrency)")

      ps.println(header.mkString(sep))
      for ((operationNumber, entry) <- this)
        ps.println(List[Any](operationNumber, entry.date.format(Format.df), entry.exchanger, entry.description, entry.costBasis, entry.proceeds, entry.fee).mkString(sep))
    }
  }
}

