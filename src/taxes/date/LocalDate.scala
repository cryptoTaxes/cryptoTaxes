package taxes.date

object LocalDate {
  def apply(year : Int, month : Int, dayOfMonth : Int) : LocalDate =
    java.time.LocalDate.of(year, month, dayOfMonth)

  def of(localDateTime : LocalDateTime) : LocalDate =
    localDateTime.toLocalDate

  import spray.json.{JsonFormat, JsString, JsValue, deserializationError}

  object localDateJson extends JsonFormat[LocalDate] {
    private val formatter = java.time.format.DateTimeFormatter.ofPattern("yyyy-MM-dd")

    def write(ld : LocalDate) = {
      JsString(formatter.format(ld))
    }

    def read(value: JsValue) = value match {
      case JsString(str) =>
        try {
          java.time.LocalDate.parse(str, formatter)
        } catch {
          case _ => deserializationError("LocalDate expected")
        }
      case _ =>
        deserializationError("LocalDate expected")
    }
  }
}