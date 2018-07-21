package taxes.util.parse

import taxes.util.Logger


object Parse {
  def asDouble(str : String) : Double = {
    val sc = new java.util.Scanner(str)
    val double = sc.nextDouble()
    sc.close()
    return double
  }

  def asInt(str : String) : Int = {
    val sc = new java.util.Scanner(str)
    val int = sc.nextInt()
    sc.close()
    return int
  }

  def asLong(str : String) : Long = {
    val sc = new java.util.Scanner(str)
    val long = sc.nextLong()
    sc.close()
    return long
  }

  def asBigDecimal(str : String) : BigDecimal = {
    val sc = new java.util.Scanner(str)
    val big = sc.nextBigDecimal()
    sc.close()
    return big
  }

  def isComment(line : String) : Boolean =
    line.isEmpty || line.startsWith("//")

  def trimSpaces(str : String) : String =
    str.dropWhile(_.isSpaceChar).reverse.dropWhile(_.isSpaceChar).reverse

  def split(str : String, separator: String) : (String, String) = {
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

  def sepBy(str0 : String, separator: String) : List[String] = {
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

  def readAssociations(fileName : String, traceMsg : String) : Map[String, String] = {
    val trace = s"$traceMsg from $fileName."
    Logger.trace(trace)
    val file = new java.io.File(fileName)
    val sc = new java.util.Scanner(file)
    var associations = Map[String, String]()
    var lineNumber = 0
    while (sc.hasNextLine) {
      val line = sc.nextLine()
      lineNumber += 1
      if(!isComment(line)) {
        try {
          val (list, normalized0) = split(line, "->")
          val normalized = normalized0.toUpperCase()
          val alternatives = sepBy(list, ",")
          for (alt <- alternatives)
            associations += (alt.toUpperCase() -> normalized)
        } catch {
          case _ => Logger.warning(s"Could not read associations in line $lineNumber '$line' from file $fileName.")
        }
      }
    }
    sc.close()
    return associations
  }
}


