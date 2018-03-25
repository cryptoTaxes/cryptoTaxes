package taxes.Util.Parse

case class ScannerException(msg : String) extends RuntimeException(msg)

trait Scanner {
  def next() : String
  def nextDouble() : Double
  def nextInt() : Int
  def close() : Unit

  def next(what : String) : String = {
    try {
      next()
    } catch {
      case _ : Exception => throw ScannerException("Error reading " + what)
    }
  }

  def nextDouble(what : String) : Double = {
    try {
      nextDouble()
    } catch {
      case _ : Exception => throw ScannerException("Error reading " + what)
    }
  }

  def nextInt(what : String) : Int = {
    try {
      nextInt()
    } catch {
      case _ : Exception => throw ScannerException("Error reading " + what)
    }
  }
}


case class QuotedScanner(str : String, delimiter : Char, sep : Char) extends Scanner {
  var string = str

  def next() : String = {
    var token = ""

    if(string.nonEmpty && string.head == sep)
      string = string.tail

    val isDelimited = string.nonEmpty && string.head == delimiter

    if(isDelimited) {
      if (string.nonEmpty && string.head == delimiter)
        string = string.tail

      while (string.nonEmpty && string.head != delimiter) {
        token += string.head
        string = string.tail
      }

      if (string.nonEmpty && string.head == delimiter)
        string = string.tail
    } else {
      while (string.nonEmpty && string.head != sep) {
        token += string.head
        string = string.tail
      }
    }

    return token
  }

  def nextDouble() : Double =
    Parse.asDouble(next())

  def nextInt() : Int =
    Parse.asInt(next())

  def close(): Unit = {}
}


case class SeparatedScanner(str : String, separatorRegex : String) extends Scanner {
  private val sc = new java.util.Scanner(str).useDelimiter(separatorRegex)

  def next() : String = sc.next()

  def nextDouble() : Double = sc.nextDouble()

  def nextInt() : Int = sc.nextInt()

  def close(): Unit = sc.close()
}
