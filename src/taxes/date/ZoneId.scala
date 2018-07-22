package taxes.date

import spray.json._


object ZoneId {
  def of(str : String) : ZoneId =
    java.time.ZoneId.of(str)

  def systemDefault() : ZoneId =
    java.time.ZoneId.systemDefault()

  object zoneIdJson extends JsonFormat[ZoneId] {
    def write(zid: ZoneId) = {
      JsString(zid.toString)
    }

    def read(value: JsValue) =
      try {
        value match {
          case JsString(str) => ZoneId.of(str)
        }
      } catch {
        case _ => deserializationError(s"ZoneId expected in $value")
      }
  }
}
