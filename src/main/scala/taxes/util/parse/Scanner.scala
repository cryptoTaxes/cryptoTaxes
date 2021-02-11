package taxes.util.parse

import scala.collection.mutable.{ArrayBuffer, ListBuffer}


case class ScannerException(msg: String) extends RuntimeException(msg)

object Scanner {
  def asDouble[T](what: String, str: String, c: Class[T]): Double = {
    try {
      Parse.asDouble(str)
    } catch {
      case _ => throw ScannerException(s"${c.getSimpleName}.nextDouble($what). token $str is not a Double")
    }
  }

  def asInt[T](what: String, str: String, c: Class[T]): Int = {
    try {
      Parse.asInt(str)
    } catch {
      case _ => throw ScannerException(s"${c.getSimpleName}.nextInt($what). token $str is not an Int")
    }
  }

  def orElse[T](get: => T, orElse: => T): T =
    try {
      get
    } catch {
      case _: Exception => orElse
    }

  def mapOrElse[T,R](get: => T, f: T => R, orElse: => R): R =
    try {
      f(get)
    } catch {
      case _: Exception => orElse
    }
}


trait Scanner {
  def close(): Unit

  def next(what: String): String

  def nextOrElse(what: String, orElse: => String): String =
    Scanner.orElse(next(what), orElse)

  def nextMapOrElse[R](what: String, f: String => R, orElse: => R): R =
    Scanner.mapOrElse(next(what), f, orElse)

  def nextDouble(what: String): Double =
    Scanner.asDouble(what, next(what), this.getClass)

  def nextDoubleOrElse(what: String, orElse: =>  Double): Double =
    Scanner.orElse(nextDouble(what), orElse)

  def nextInt(what: String): Int =
    Scanner.asInt(what, next(what), this.getClass)

  def nextIntOrElse(what: String, orElse: =>  Int): Int =
    Scanner.orElse(nextInt(what), orElse)
}


trait SequentialScanner extends Scanner {
  def hasNext(): Boolean
}

case class QuotedScanner(input: String, delimiter: Char, sep: Char) extends SequentialScanner {
  private val end = input.length
  private var i = 0
  private var firstToken = true

  override def hasNext(): Boolean =
    i < end

  override def next(what: String): String = {
    if(!hasNext())
      throw new ScannerException(s"${this.getClass.getSimpleName}: no more tokens scanning for $what")

    // skip separator before token, except for first token
    if(!firstToken)
      i += 1

    firstToken = false

    val isDelimited = i < end && input(i) == delimiter

    if(isDelimited) {
      val begin = i
      var closed = false

      while(!closed) {
        val j = input.indexOf(delimiter, i+1)
        if(j < 0) // closing delimiter not found
          throw new ScannerException(s"${this.getClass.getSimpleName}: non-closed delimited token ${input.substring(begin)} scanning for $what")
        else if(input(j-1) == '\\') // escaped delimiter
          i = j+1
        else if (j+1 < end && input(j+1) == delimiter) // two consecutive delimiters
          i = j+1
        else {
          i = j+1
          closed = true
        }
      }
      return input.substring(begin+1, i-1) // do not include quotes
    } else {
      val begin = i
      val j = input.indexOf(sep, i)
      i = if(j < 0) end else j
      return input.substring(begin, i)
    }
  }
  override def close(): Unit = {}
}

case class SeparatedScanner(str: String, separatorRegex: String) extends SequentialScanner {
  private val sc = new java.util.Scanner(str).useDelimiter(separatorRegex)

  override def next(what: String): String = sc.next()

  override def hasNext(): Boolean = sc.hasNext()

  def close(): Unit = sc.close()
}

trait AssociativeScannerProvider {
  def baseScannerFor(input: String): SequentialScanner

  def keysLine: String

  private val indexOfKey = {
    var map = collection.immutable.Map[String, Int]()
    val sc = baseScannerFor(keysLine)
    var i = 0
    while(sc.hasNext()) {
      val key = sc.next("key")
      map = map + (key -> i)
      i += 1
    }
    sc.close()
    map
  }

  def keys: Iterable[String] = indexOfKey.keys

  def scannerFor(input: String): Scanner = new Scanner {
    private val array = {
      val arrayBuffer = new ArrayBuffer[String]()
      val sc = baseScannerFor(input)
      var i = 0
      while (sc.hasNext()) {
        arrayBuffer += sc.next(s"value at column $i")
        i += 1
      }
      sc.close()
      arrayBuffer.toArray
    }

    override def close(): Unit = {}

    override def next(what: String): String = {
      val idx = try {
        indexOfKey(what)
      } catch {
        case _ => throw ScannerException(s"${this.getClass.getSimpleName}. Key $what is not defined. Defined keys are ${keys.mkString(", ")}")
      }
      try {
        array(idx)
      } catch {
        case _ => throw ScannerException(s"${this.getClass.getSimpleName}. value for key $what is not defined")
      }
    }
  }
}

case class AssociativeQuotedScannerProvider(keysLine: String, delimiter: Char, sep: Char) extends AssociativeScannerProvider {
  override def baseScannerFor(input: String): SequentialScanner =
    QuotedScanner(input, delimiter, sep)
}

case class AssociativeSeparatedScannerProvider(keysLine: String, separatorRegex: String) extends AssociativeScannerProvider {
  override def baseScannerFor(input: String): SequentialScanner =
    SeparatedScanner(input, separatorRegex)
}
