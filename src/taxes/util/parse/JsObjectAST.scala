package taxes.util.parse

import spray.json.{JsArray, JsNumber, JsObject, JsString, JsValue, JsonFormat}
import taxes.FileSystem

case class JSONException(msg : String) extends RuntimeException(msg)

object JsObjectAST {
  def apply(fields: Map[String, JsValue]): JsObjectAST =
    new JsObjectAST(fields)

  def apply(jsObject: JsObject): JsObjectAST =
    JsObjectAST(jsObject.fields)

  def fromJsValue(jsValue: JsValue): JsObjectAST =
    JsObjectAST(jsValue.asJsObject)

  def fromString(str: String): JsObjectAST =
    JsObjectAST(spray.json.JsonParser(str).asJsObject())

  def fromFile(fileName: String): JsObjectAST =
    FileSystem.withSource(fileName){ src => JsObjectAST.fromString(src.mkString) }

  def fromFile(file: java.io.File): JsObjectAST =
    FileSystem.withSource(file){ src => JsObjectAST.fromString(src.mkString) }
}

class JsObjectAST(override val fields: Map[String, JsValue]) extends JsObject(fields) {
  def getString(key: String): String =
    getFields(key) match {
      case Seq(JsString(str)) =>
        str
      case Seq(_) =>
        throw JSONException("JsonAST.getString: key %s is not a string.".format(key))
      case _ =>
        throw JSONException("JsonAST.getString: key %s not found.".format(key))
    }

  def getInt(key: String): Int =
    getFields(key) match {
      case Seq(JsNumber(number)) =>
        number.intValue()
      case Seq(JsString(str)) =>
        try {
          Parse.asInt(str)
        } catch {
          case _ => throw JSONException("JsonAST.getInt: key %s is not a number.".format(key))
        }
      case Seq(_) =>
        throw JSONException("JsonAST.getInt: key %s is not a number.".format(key))
      case _ =>
        throw JSONException("JsonAST.getInt: key %s not found.".format(key))
    }

  def getLong(key: String): Long =
    getFields(key) match {
      case Seq(JsNumber(number)) =>
        number.longValue()
      case Seq(JsString(str)) =>
        try {
          Parse.asLong(str)
        } catch {
          case _ => throw JSONException("JsonAST.getLong: key %s is not a number.".format(key))
        }
      case Seq(_) =>
        throw JSONException("JsonAST.getLong: key %s is not a number.".format(key))
      case _ =>
        throw JSONException("JsonAST.getLong: key %s not found.".format(key))
    }

  def getDouble(key: String): Double =
    getFields(key) match {
      case Seq(JsNumber(number)) =>
        number.doubleValue()
      case Seq(JsString(str)) =>
        try {
          Parse.asDouble(str)
        } catch {
          case _ => throw JSONException("JsonAST.getDouble: key %s is not a number.".format(key))
        }
      case Seq(_) =>
        throw JSONException("JsonAST.getDouble: key %s is not a number.".format(key))
      case _ =>
        throw JSONException("JsonAST.getDouble: key %s not found.".format(key))
    }

  def getVector(key: String): Seq[JsValue] =
    getFields(key) match {
      case Seq(JsArray(array)) =>
        array
      case Seq(_) =>
        throw JSONException("JsonAST.getVector: key %s is not a vector.".format(key))
      case _ =>
        throw JSONException("JsonAST.getVector: key %s not found.".format(key))
    }
}



