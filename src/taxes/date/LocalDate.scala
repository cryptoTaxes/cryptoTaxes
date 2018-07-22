package taxes.date

import spray.json._


object LocalDate {
  def apply(year : Int, month : Int, dayOfMonth : Int) : LocalDate =
    java.time.LocalDate.of(year, month, dayOfMonth)

  def of(localDateTime : LocalDateTime) : LocalDate =
    localDateTime.toLocalDate

  object localDateJson extends JsonFormat[LocalDate] {
    private val formatter = java.time.format.DateTimeFormatter.ofPattern("yyyy-MM-dd")

    def write(ld : LocalDate) = {
      JsString(formatter.format(ld))
    }

    def read(value: JsValue) =
      try {
        value match {
          case JsString(str) =>
            java.time.LocalDate.parse(str, formatter)
        }
      } catch {
        case _ => deserializationError(s"LocalDate expected in $value")
      }
  }
}