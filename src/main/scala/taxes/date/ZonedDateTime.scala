package taxes.date

object ZonedDateTime {

  import java.time.format.DateTimeFormatter

  def of(localDateTime: LocalDateTime, zoneId: ZoneId): ZonedDateTime =
    java.time.ZonedDateTime.of(localDateTime, zoneId)

  def parse(str: CharSequence, format: DateTimeFormatter): ZonedDateTime = {
    val zonedDateTime = java.time.ZonedDateTime.parse(str, format)
    return zonedDateTime
  }

  def parse(str: String, format: String): ZonedDateTime =
    parse(str, DateTimeFormatter.ofPattern(format))
}