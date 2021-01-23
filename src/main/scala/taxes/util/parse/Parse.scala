package taxes.util.parse

import taxes.io.FileSystem
import taxes.util.Logger


object Parse {
  def asDouble(str: String): Double = {
    val sc = new java.util.Scanner(str)
    val double = sc.nextDouble()
    sc.close()
    return double
  }

  def asInt(str: String): Int = {
    val sc = new java.util.Scanner(str)
    val int = sc.nextInt()
    sc.close()
    return int
  }

  def asLong(str: String): Long = {
    val sc = new java.util.Scanner(str)
    val long = sc.nextLong()
    sc.close()
    return long
  }

  def asBigDecimal(str: String): BigDecimal = {
    val sc = new java.util.Scanner(str)
    val big = sc.nextBigDecimal()
    sc.close()
    return big
  }

  def isComment(line: String): Boolean =
    line.isEmpty || line.startsWith("//")

  def trimSpaces(str: String): String =
    str.dropWhile(_.isSpaceChar).reverse.dropWhile(_.isSpaceChar).reverse

  def split(str: String, separator: String): (String, String) = {
    val idx = str.indexOf(separator)
    if(idx < 0) {
      Logger.fatal(s"Separator $separator not found in pair '$str'")
    } else {
      val token1 = trimSpaces(str.take(idx))
      val token2 = trimSpaces(str.drop(idx+separator.length))
      if(token2.isEmpty)
        Logger.fatal(s"Second token not found in pair '$str'")

      return (token1, token2)
    }
  }

  def sepBy(str0: String, separator: String): List[String] = {
    var str = str0
    var list = List[String]()
    var idx = str.indexOf(separator)
    while(idx > 0) {
      val token = str.take(idx)
      str = str.drop(idx+separator.length)
      list ::= token
      idx = str.indexOf(separator)
    }
    list ::= str
    return list.map(trimSpaces).reverse
  }

  def removePrefix(str: String, prefix: String): Option[String] =
    if(str.startsWith(prefix))
      Some(str.drop(prefix.length))
    else
      None

  def removeSuffix(str: String, suffix: String): Option[String] =
    if(str.endsWith(suffix))
      Some(str.take(str.length - suffix.length))
    else
      None

  def unquote(str: String, delimiter: String): Option[String] =
    for {
      str1 <- removePrefix(str, delimiter)
    ; str2 <- {
        val idx = str1.indexOf(delimiter)
        if(idx < 0) None else Some(str1.take(idx))
      }
    } yield (str2)

  def skipUntil(str: String, prefix: String): Option[String] = {
    val idx = str.indexOf(prefix)
    if(idx < 0)
      None
    else
      Parse.removePrefix(str.drop(idx), prefix)
  }

  private val arrowToken = "->"
  private val commaToken = ","

  def readKeysValue(fileName: String, traceMsg: String): Map[String, String] = {
    var associations = Map[String, String]()
    val file = FileSystem.File(fileName)
    if(file.exists()) {
      val trace = s"$traceMsg from $fileName."
      Logger.trace(trace)
      val sc = new java.util.Scanner(file, taxes.io.defaultCharset.name())

      var lineNumber = 0
      while(sc.hasNextLine) {
        val line = sc.nextLine()
        lineNumber += 1
        if(!isComment(line)) {
          try {
            val (list, value) = split(line, arrowToken)
            val keys = sepBy(list, commaToken)
            for(key <- keys)
              associations += (key -> value)
          } catch {
            case _ => Logger.warning(s"Could not read inverse associations in line $lineNumber '$line' from file $fileName.")
          }
        }
      }
      sc.close()
    }
    return associations
  }

  def readKeyValues(fileName: String, traceMsg: String): Map[String, Iterable[String]] = {
    var associations = Map[String, Iterable[String]]()
    val file = FileSystem.File(fileName)
    if(file.exists()) {
      val trace = s"$traceMsg from $fileName."
      Logger.trace(trace)
      val sc = new java.util.Scanner(file, taxes.io.defaultCharset.name())

      var lineNumber = 0
      while(sc.hasNextLine) {
        val line = sc.nextLine()
        lineNumber += 1
        if(!isComment(line)) {
          try {
            val (key, list) = split(line, arrowToken)
            val values = sepBy(list, commaToken)
            associations += (key -> values)
          } catch {
            case _ => Logger.warning(s"Could not read associations in line $lineNumber '$line' from file $fileName.")
          }
        }
      }
      sc.close()
    }
    return associations
  }
}


