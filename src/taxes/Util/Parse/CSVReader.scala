package taxes.Util.Parse

import taxes.Util.Logger
import taxes.Operation

trait CSVReader[A] {
  val hasHeader : Boolean

  def lineScanner(line : String ) : Scanner

  def readLine(line : String, lineScanner : Scanner) : Either[String,A]

  def readFile(fileName : String) : List[A] = {
    val f = new java.io.File(fileName)
    val sc = new java.util.Scanner(f)

    var xs = List[A]()

    if(hasHeader)
      sc.nextLine()

    while(sc.hasNextLine) {
      val ln = Parse.trimSpaces(sc.nextLine())
      if(ln.nonEmpty) {
        val scLn = lineScanner(ln)
        val opt = readLine(ln, scLn)
        scLn.close()

        opt match {
          case Right(x) =>
            xs ::= x
          case Left(err) =>
            Logger.warning(err)
        }
      }
    }
    sc.close()
    return xs
  }
}


abstract class CSVSortedOperationReader extends CSVReader[Operation] {
  override def readFile(fileName : String) : List[Operation] =
    super.readFile(fileName).sortBy(_.date)
}