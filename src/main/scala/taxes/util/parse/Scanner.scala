package taxes.util.parse

import scala.collection.mutable.{ArrayBuffer, ListBuffer}


case class ScannerException(msg: String) extends RuntimeException(msg)


trait Scanner {
  def close(): Unit

  def next(what: String): String

  def nextOrElse(what: String, orElse: => String): String =
    try {
      next(what)
    } catch {
      case _: Exception => orElse
    }

  def nextDouble(what: String): Double

  def nextDoubleOrElse(what: String, orElse: =>  Double): Double =
    try {
      nextDouble(what)
    } catch {
      case _: Exception => orElse
    }

  def nextInt(what: String): Int

  def nextIntOrElse(what: String, orElse: =>  Int): Int =
    try {
      nextInt(what)
    } catch {
      case _: Exception => orElse
    }
}


case class QuotedScanner(str: String, delimiter: Char, sep: Char) extends Scanner {
  private var string = str

  override def next(what: String): String = {
    var token = ""

    if(string.nonEmpty && string.head == sep)
      string = string.tail

    val isDelimited = string.nonEmpty && string.head == delimiter

    if(isDelimited) {
      if(string.nonEmpty && string.head == delimiter)
        string = string.tail

      while(string.nonEmpty && string.head != delimiter) {
        token += string.head
        string = string.tail
      }

      if(string.nonEmpty && string.head == delimiter)
        string = string.tail
    } else {
      while(string.nonEmpty && string.head != sep) {
        token += string.head
        string = string.tail
      }
    }

    return token
  }

  override def nextDouble(what: String): Double =
    Parse.asDouble(next(what))

  override def nextInt(what: String): Int =
    Parse.asInt(next(what))

  def close(): Unit = {}
}


case class SeparatedScanner(str: String, separatorRegex: String) extends Scanner {
  private val sc = new java.util.Scanner(str).useDelimiter(separatorRegex)

  override def next(what: String): String = sc.next()

  override def nextDouble(what: String): Double = sc.nextDouble()

  override def nextInt(what: String): Int = sc.nextInt()

  def close(): Unit = sc.close()
}


object AssociativeSeparatedScanner {
  def apply(keys: String, separatorRegex: String): String => Scanner = {
    val indexOf = {
      val keysSc = new java.util.Scanner(keys).useDelimiter(separatorRegex)
      val indexes = ListBuffer[String]()
      while(keysSc.hasNext())
        indexes += keysSc.next()
      keysSc.close()
      indexes.zipWithIndex.toMap
    }

    return (line: String) => new Scanner {
      val array = {
        val sc = new java.util.Scanner(line).useDelimiter(separatorRegex)
        val arrayBuffer = new ArrayBuffer[String]()
        while (sc.hasNext())
          arrayBuffer += sc.next()
        sc.close()
        arrayBuffer.toArray
      }

      override def close(): Unit = {}

      private def value(what: String, method: String): String = {
        val idx = try {
          indexOf(what)
        } catch {
          case _ => throw ScannerException(s"AssociativeSeparatedScanner.$method($what). Key not defined")
        }
        try {
          array(idx)
        } catch {
          case _ => throw ScannerException(s"AssociativeSeparatedScanner.$method($what). value not defined")
        }
      }

      override def next(what: String): String = value(what, "next")

      override def nextDouble(what: String): Double = {
        val v = value(what, "nextDouble")
        try {
          Parse.asDouble(v)
        } catch {
          case _ => throw ScannerException(s"AssociativeSeparatedScanner.nextDouble($what). value is not a Double")
        }
      }

      override def nextInt(what: String): Int = {
        val v = value(what, "nextInt")
        try {
          Parse.asInt(v)
        } catch {
          case _ => throw ScannerException(s"AssociativeSeparatedScanner.nextInt($what). value is not an Int")
        }
      }
    }
  }
}