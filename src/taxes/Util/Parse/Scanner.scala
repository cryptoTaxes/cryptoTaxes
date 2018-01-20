package taxes.Util.Parse

trait Scanner {
  def next() : String
  def nextDouble() : Double
  def nextInt() : Int
  def close() : Unit
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

  def nextDouble() : Double = {
    val sc = new java.util.Scanner(next())
    val double = sc.nextDouble()
    sc.close()
    return double
  }

  def nextInt() : Int = {
    val sc = new java.util.Scanner(next())
    val int = sc.nextInt()
    sc.close()
    return int
  }

  def close(): Unit = {}
}


case class SeparatedScanner(str : String, delimiterRegex : String) extends Scanner {
  private val sc = new java.util.Scanner(str).useDelimiter(delimiterRegex)

  def next() : String = sc.next()

  def nextDouble() : Double = sc.nextDouble()

  def nextInt() : Int = sc.nextInt()

  def close(): Unit = sc.close()
}
