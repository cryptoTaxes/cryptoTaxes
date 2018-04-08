package taxes.Util.Parse

case class JSONException(msg : String) extends RuntimeException(msg)

trait JSONParser {
  def getString(key : String) : String

  def getDouble(key : String) : Double

  def getInt(key : String) : Int
}


case class SimpleJSONParser(str0 : String) extends JSONParser {
  private val map = scala.collection.mutable.Map[String, String]()

  def skipSpaces(): Unit = {
    while(str.nonEmpty && str.head.isSpaceChar)
      str = str.tail
  }

  var str = str0
  skipSpaces()
  if(str.head != '{')
    throw JSONException("character { not found.")
  str = str.tail.reverse

  skipSpaces()
  if(str.head != '}')
    throw JSONException("character } not found.")
  str = str.tail.reverse


  val delimiter = '\"'
  val keyValSep = ':'
  val sep = ','


  var end = false
  while(!end) {
    skipSpaces()

    if(str.isEmpty)
      end = true
    else {
      var key = ""
      val isDelimitedKey = str.head == delimiter
      if(isDelimitedKey) {
        str = str.tail // skip delimiter
        while(str.head != delimiter) {
          key = key + str.head
          str = str.tail
        }
        str = str.tail  // skip delimiter
      } else
        throw JSONException("key should be delimited "+str)

      skipSpaces()
      if(str.isEmpty)
        throw JSONException("key without corresponding value")

      if(str.head != keyValSep)
        throw JSONException("key/value separator expected: %s".format(str))

      str = str.tail // skip keyValSep

      var value = ""
      val isDelimitedVal = str.head == delimiter
      if(isDelimitedVal) {
        str = str.tail // skip delimiter
        while(str.head != delimiter) {
          value = value + str.head
          str = str.tail
        }
        str = str.tail  // skip delimiter
      } else {
        while (str.nonEmpty && str.head != sep) {
          value = value + str.head
          str = str.tail
        }

      }
      if (str.nonEmpty)
        str = str.tail // skip sep
      else
        end = true

      map += (key -> value)
    }
  }

  def getString(key : String) : String = map.get(key) match {
    case None        => throw JSONException("key %s not found.".format(key))
    case Some(value) => value
  }

  def getDouble(key : String) : Double =
    return Parse.asDouble(getString(key))

  def getInt(key : String) : Int =
    Parse.asInt(getString(key))
}


object AdvancedJSONParser {
  def apply(str : String) : AdvancedJSONParser =
    new AdvancedJSONParser(str)
}


case class AdvancedJSONParser(map : Map[String, Any]) extends JSONParser {
  def this(str : String) {
    this(scala.util.parsing.json.JSON.parseFull(str) match {
      case None      => throw JSONException("AdvancedJSONParser: parse failed for %s".format(str))
      case Some(map) => map.asInstanceOf[Map[String, Any]]
    })
  }

  def getString(key : String) = map(key).asInstanceOf[String]

  def getDouble(key: String) = {
    map(key) match {
      case int : Int       => int.toDouble
      case double : Double => double
      case str : String    => Parse.asDouble(str)
      case obj             => throw JSONException("AdvancedJSONParser.getDouble: not a Double %s".format(obj))
    }
  }

  def getInt(key: String) = {
    map(key) match {
      case int : Int       => int
      case str : String    => Parse.asInt(str)
      case obj             => throw JSONException("AdvancedJSONParser.getInt: not an Int %s".format(obj))
    }
  }

  def apply[A](key : String) : A =
    map(key).asInstanceOf[A]

  def getList(key : String) = {
    map(key) match {
      case list : List[Map[String,Any]] =>
        list.map(AdvancedJSONParser(_))
      case obj             =>
        throw JSONException("AdvancedJSONParser.getList: not a List %s".format(obj))
    }
  }
}