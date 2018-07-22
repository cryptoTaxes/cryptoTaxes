package taxes

package object date  {
  type LocalDate = java.time.LocalDate

  implicit val localDateJson = LocalDate.localDateJson

  implicit class LocalDateOps(private val localDate: LocalDate) extends AnyVal with Ordered[LocalDate] {
    override def compare(that: LocalDate): Int =
      localDate.compareTo(that)
  }

  type LocalDateTime = java.time.LocalDateTime

  implicit class LocalDateTimeOps(private val localDateTime: LocalDateTime) extends AnyVal with Ordered[LocalDateTime] {
    import java.time.temporal.ChronoUnit

    override def compare(that: LocalDateTime): Int =
      localDateTime.compareTo(that)

    def sameDayAs(that : LocalDateTime) : Boolean =
      localDateTime.truncatedTo(ChronoUnit.DAYS).equals(that.truncatedTo(ChronoUnit.DAYS))
  }

  implicit val localDateTimeJson = LocalDateTime.localDateTimeJson

  type ZonedDateTime = java.time.ZonedDateTime

  type ZoneId = java.time.ZoneId

  implicit val zoneIdJson = ZoneId.zoneIdJson
}
