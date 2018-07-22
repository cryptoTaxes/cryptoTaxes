package spray.json


trait EnumFormats {
  implicit def jsonEnumFormat[T <: scala.Enumeration](enu: T) = new RootJsonFormat[T#Value] {
    override def write(obj: T#Value): JsValue = JsString(obj.toString)

    override def read(json: JsValue): T#Value =
      try {
        json match {
          case JsString(txt) => enu.withName(txt)
        }
      } catch {
        case _ => throw DeserializationException(s"Expected a value from enum $enu instead of $json")
      }
  }
}



