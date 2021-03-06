package taxes.util.parse

import taxes.date._
import taxes.io.FileSystem
import taxes.util._
import taxes.{FileSource, Operation}

import scala.collection.mutable.ListBuffer


object CSVReader {
  class Result[+A]

  object Ok {
    def apply[A](result: A) =
      new Ok(List(result))
  }

  case class Ok[+A](results: Seq[A]) extends Result[A]
  case class Warning(msg: String) extends Result[Nothing]
  case object Ignore extends Result[Nothing]
}

abstract class CSVReader[A](fileName: String) extends FileSource[A](fileName) {
  import CSVReader._

  // number of non-empty lines to skip before data
  val linesToSkip: Int

  lazy val skippedLines = new Array[String](linesToSkip)

  val charset: String = taxes.io.defaultCharset.name

  def lineScanner(line: String): Scanner

  def readLine(line: String, lineScanner: Scanner): Result[A]

  def read(): List[A] = {
    val f = FileSystem.File(fileName)
    val sc = new java.util.Scanner(f, charset)

    val xs = ListBuffer[A]()
    var lnNumber = 0

    var skipped = 0
    while(skipped < linesToSkip) {
      lnNumber += 1
      val ln = Parse.trimSpaces(sc.nextLine())
      if(!Parse.isComment(ln)) {
        skippedLines(skipped) = ln
        skipped += 1
      }
    }

    while(sc.hasNextLine) {
      lnNumber += 1
      val ln = Parse.trimSpaces(sc.nextLine())
      if(!Parse.isComment(ln)) {
        val scLn = lineScanner(ln)
        try {
          readLine(ln, scLn) match {
            case Ok(ys) =>
              for(y <- ys)
                xs += y
            case Warning(err) =>
              Logger.warning(err)
            case Ignore =>
              ;
          }
        } catch {
          case e =>
            Logger.fatal(s"Something went wrong reading csv file $fileName.\nLine $lnNumber: $ln\n$e")
        } finally
          scLn.close()
      }
    }
    sc.close()
    return xs.toList
  }
}


abstract class CSVSortedOperationReader(fileName: String) extends CSVReader[Operation](fileName) {
  override def read(): List[Operation] =
    super.read().sortBy(_.date)
}
