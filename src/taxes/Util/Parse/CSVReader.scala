package taxes.Util.Parse

import taxes.Util.Logger
import taxes.{FileSource, Operation}

abstract class CSVReader[A](fileName : String) extends FileSource[A](fileName) {
  val hasHeader : Boolean

  def lineScanner(line : String ) : Scanner

  def readLine(line : String, lineScanner : Scanner) : Either[String,A]

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
            case Right(x) =>
              xs ::= x
            case Left(err) =>
              Logger.warning(err)
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