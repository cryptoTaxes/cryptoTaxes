package taxes.Util.Parse

import taxes.Util.Logger
import taxes.{FileSource, Operation}

object CSVReader {
  class Result[+A]

  case class Ok[+A](result : A) extends Result[A]
  case class Warning(msg : String) extends Result[Nothing]
  case object Ignore extends Result[Nothing]
}

abstract class CSVReader[A](fileName : String) extends FileSource[A](fileName) {
  import CSVReader._

  val hasHeader : Boolean

  def lineScanner(line : String) : Scanner

  def readLine(line : String, lineScanner : Scanner) : Result[A]

  def read() : List[A] = {
    val f = new java.io.File(fileName)
    val sc = new java.util.Scanner(f)

    var xs = List[A]()

    if(hasHeader)
      sc.nextLine()

    while(sc.hasNextLine) {
      val ln = Parse.trimSpaces(sc.nextLine())
      if(ln.nonEmpty) {
        val scLn = lineScanner(ln)
        try {
          readLine(ln, scLn) match {
            case Ok(x) =>
              xs ::= x
            case Warning(err) =>
              Logger.warning(err)
            case Ignore =>
              ;
          }
        } catch {
          case e => Logger.fatal("Something went wrong reading csv file %s. %s".format(fileName, e))
        } finally
          scLn.close()
      }
    }
    sc.close()
    return xs
  }
}


abstract class CSVSortedOperationReader(fileName : String) extends CSVReader[Operation](fileName) {
  override def read() : List[Operation] =
    super.read().sortBy(_.date)
}