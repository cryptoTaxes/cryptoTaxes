package taxes.util.parse


import spray.json._
import taxes.io.FileSystem

case class JSONException(msg: String) extends RuntimeException(msg)

object JsObjectAST {
  def apply(fields: Map[String, JsValue]): JsObjectAST =
    new JsObjectAST(fields)

  def apply(jsObject: JsObject): JsObjectAST =
    JsObjectAST(jsObject.fields)

  def fromJsValue(jsValue: JsValue): JsObjectAST =
    JsObjectAST(jsValue.asJsObject)

  def fromString(str: String): JsObjectAST =
    JsObjectAST(str.parseJson.asJsObject())

  def fromFile(file: FileSystem.File): JsObjectAST =
    FileSystem.withSource(file){ src => JsObjectAST.fromString(src.mkString) }

  def fromFile(fileName: String): JsObjectAST =
    fromFile(FileSystem.File(fileName))

}

class JsObjectAST(override val fields: Map[String, JsValue]) extends JsObject(fields) {
  def getString(key: String): String =
    getFields(key) match {
      case Seq(JsString(str)) =>
        str
      case Seq(_) =>
        throw JSONException(s"JsObjectAST.getString: key $key is not a string.")
      case _ =>
        throw JSONException(s"JsObjectAST.getString: key $key not found.")
    }

  def getInt(key: String): Int =
    getFields(key) match {
      case Seq(JsNumber(number)) =>
        number.intValue()
      case Seq(JsString(str)) =>
        try {
          Parse.asInt(str)
        } catch {
          case _ => throw JSONException(s"JsObjectAST.getInt: key $key is not a number.")
        }
      case Seq(_) =>
        throw JSONException(s"JsObjectAST.getInt: key $key is not a number.")
      case _ =>
        throw JSONException(s"JsObjectAST.getInt: key $key not found.")
    }

  def getLong(key: String): Long =
    getFields(key) match {
      case Seq(JsNumber(number)) =>
        number.longValue()
      case Seq(JsString(str)) =>
        try {
          Parse.asLong(str)
        } catch {
          case _ => throw JSONException(s"JsObjectAST.getLong: key $key is not a number.")
        }
      case Seq(_) =>
        throw JSONException(s"JsObjectAST.getLong: key $key is not a number.")
      case _ =>
        throw JSONException(s"JsObjectAST.getLong: key $key not found.")
    }

  def getDouble(key: String): Double =
    getFields(key) match {
      case Seq(JsNumber(number)) =>
        number.doubleValue()
      case Seq(JsString(str)) =>
        try {
          Parse.asDouble(str)
        } catch {
          case _ => throw JSONException(s"JsObjectAST.getDouble: key $key is not a number.")
        }
      case Seq(_) =>
        throw JSONException(s"JsObjectAST.getDouble: key $key is not a number.")
      case _ =>
        throw JSONException(s"JsObjectAST.getDouble: key $key not found.")
    }

  def getVector(key: String): Seq[JsValue] =
    getFields(key) match {
      case Seq(JsArray(array)) =>
        array
      case Seq(_) =>
        throw JSONException(s"JsObjectAST.getVector: key $key is not a vector.")
      case _ =>
        throw JSONException(s"JsObjectAST.getVector: key $key not found.")
    }

  def getObjectAST(key: String): JsObjectAST =
    getFields(key) match {
      case Seq(jsValue) =>
        try {
          JsObjectAST(jsValue.asJsObject())
        } catch {
          case _ =>
            throw JSONException(s"JsObjectAST.getObjectAST: key $key is not an object.")
        }
      case _ =>
        throw JSONException(s"JsObjectAST.getObjectAST: key $key not found.")
    }
}



