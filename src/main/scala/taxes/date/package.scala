package taxes

package object date  {
  type LocalDate = java.time.LocalDate

  implicit val localDateJson = LocalDateJson

  implicit class LocalDateOps(private val localDate: LocalDate) extends AnyVal with Ordered[LocalDate] {
    override def compare(that: LocalDate): Int =
      localDate.compareTo(that)
  }

  type LocalDateTime = java.time.LocalDateTime

  implicit class LocalDateTimeOps(private val localDateTime: LocalDateTime) extends AnyVal with Ordered[LocalDateTime] {
    import java.time.temporal.ChronoUnit

    override def compare(that: LocalDateTime): Int =
      localDateTime.compareTo(that)

    def sameDayAs(that: LocalDateTime): Boolean =
      localDateTime.truncatedTo(ChronoUnit.DAYS).equals(that.truncatedTo(ChronoUnit.DAYS))

    def atLeast1YearFrom(fromDateTime: LocalDateTime): Boolean = {
      val toDateTime = this.localDateTime
      var tempDateTime = java.time.LocalDateTime.from(fromDateTime)

      val years = tempDateTime.until(toDateTime, ChronoUnit.YEARS)
      tempDateTime = tempDateTime.plusYears(years)

      val days = tempDateTime.until(toDateTime, ChronoUnit.DAYS)

      return (years>=1 && days>=1)
    }

    def difference(fromDateTime: LocalDateTime): LocalDateTimeDiff = {
      // taken from https://stackoverflow.com/questions/25747499/java-8-difference-between-two-localdatetime-in-multiple-units
      import java.time.temporal.ChronoUnit

      val toDateTime = this.localDateTime
      var tempDateTime = java.time.LocalDateTime.from(fromDateTime)

      val years = tempDateTime.until(toDateTime, ChronoUnit.YEARS)
      tempDateTime = tempDateTime.plusYears(years)

      val months = tempDateTime.until(toDateTime, ChronoUnit.MONTHS)
      tempDateTime = tempDateTime.plusMonths(months)

      val days = tempDateTime.until(toDateTime, ChronoUnit.DAYS)
      tempDateTime = tempDateTime.plusDays(days)

      val hours = tempDateTime.until(toDateTime, ChronoUnit.HOURS)
      tempDateTime = tempDateTime.plusHours(hours)

      val minutes = tempDateTime.until(toDateTime, ChronoUnit.MINUTES)
      tempDateTime = tempDateTime.plusMinutes(minutes)

      val seconds = tempDateTime.until(toDateTime, ChronoUnit.SECONDS)
      return LocalDateTimeDiff(years, months, days, hours, minutes, seconds)
    }
  }

  implicit val localDateTimeJson = LocalDateTimeJson

  type ZonedDateTime = java.time.ZonedDateTime

  type ZoneId = java.time.ZoneId

  implicit val zoneIdJson = ZoneIdJson
}
