package taxes.Util.Parse

import taxes.Util.Logger

package object Parse {

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

  def isComment(line : String) : Boolean =
    line.isEmpty || line.startsWith("//")

  def trimSpaces(str : String) : String =
    str.dropWhile(_.isSpaceChar).reverse.dropWhile(_.isSpaceChar).reverse

  def split(str : String, separator: String) : (String, String) = {
    val idx = str.indexOf(separator)
    if(idx < 0) {
      Logger.fatal("Separator %s not found in pair \"%s\"" format(separator, str))
    } else {
      val token1 = trimSpaces(str.take(idx))
      val token2 = trimSpaces(str.drop(idx+separator.length))
      if(token2.isEmpty)
        Logger.fatal("Second token not found in pair \"%s\"" format(str))

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
    val trace = traceMsg + " from " + fileName + "."
    Logger.trace(trace)
    val file = new java.io.File(fileName)
    val sc = new java.util.Scanner(file)
    var associations = Map[String, String]()
    var lineNumber = 0
    while (sc.hasNextLine) {
      val line = sc.nextLine()
      lineNumber += 1
      if(!Parse.isComment(line)) {
        try {
          val (list, normalized0) = Parse.split(line, "->")
          val normalized = normalized0.toUpperCase()
          val alternatives = Parse.sepBy(list, ",")
          for (alt <- alternatives)
            associations += (alt.toUpperCase() -> normalized)
        } catch {
          case _ => Logger.warning("Could not read associations in line %d \"%s\" from file %s." format(lineNumber, line, fileName))
        }
      }
    }
    sc.close()
    return associations
  }
}


