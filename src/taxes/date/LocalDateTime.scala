package taxes.date

import taxes.Config

object LocalDateTime {
  import java.time.format.DateTimeFormatter
  import java.time.{Instant, ZoneId}

  // LocalDateTimes are in myZoneId
  lazy val myZoneId : ZoneId = Config.config.timeZone // ZoneId.of("Europe/Madrid") // ZoneId.systemDefault()

  def apply(year : Int, month: Int, day : Int) : LocalDateTime =
    java.time.LocalDateTime.of(year, month, day, 0, 0, 0)

  def apply() : LocalDateTime =
    apply(0, 1, 1)

  def fromZonedDateTime(zonedDateTime: ZonedDateTime) : LocalDateTime =
    zonedDateTime.withZoneSameInstant(myZoneId).toLocalDateTime

  def fromUnix(seconds : Long) : LocalDateTime = {
    val instant = Instant.ofEpochSecond(seconds)
    val zonedDateTime = java.time.ZonedDateTime.ofInstant(instant, myZoneId)
    return fromZonedDateTime(zonedDateTime)
  }

  def parse(str : CharSequence, format : DateTimeFormatter) : LocalDateTime = {
    val zonedDateTime = ZonedDateTime.parse(str, format)
    return fromZonedDateTime(zonedDateTime)
  }

  private val formatter = java.time.format.DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.SSSS VV")

  def parse(str : String) : LocalDateTime =
    parse(str, formatter)

  def parse(str : String, format : String) : LocalDateTime =
    parse(str, DateTimeFormatter.ofPattern(format))

  private val UTC_Offset = "+0000"

  def parseAsUTC(str : String, format : String) : LocalDateTime =
    parse(str+UTC_Offset, format+"Z")

  def parseAsMyZoneId(str : String, format: String) : LocalDateTime =
    parse(str+myZoneId, format+"VV")
}


object TestLocalDateTime extends App {

  val formatter = java.time.format.DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.[SSSS][SSS]Z")
  val formatter2 = java.time.format.DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")

  val date1 = LocalDateTime.parse("2017-11-29 21:55:42.123+0000", formatter)
  println(date1)
  println(date1.format(formatter2))


  val date2 = LocalDateTime.parse("2017-11-28 23:14:32.6213+0100", formatter)
  println(date2)
  println(date2.format(formatter2))


  val date3 = LocalDateTime.parse("2017-11-29 21:55:42.123+0200", formatter)
  println(date3)
  println(date3.format(formatter2))


  val date4 = LocalDateTime.parse("2017-08-29 21:55:42.123+0000", formatter)
  println(date4)

  println(date4.plusDays(1))


  println()
  val date5 = LocalDateTime.parse("2017-12-31 23:59:59.000+0100", formatter)
  println(date5)
  val date6 = date5.plusSeconds(1)
  println(date6)
  println(date6.getYear)


  val date7 = LocalDateTime.fromUnix(1531075102L)
  println(date7)

  val date8 = LocalDateTime.parseAsUTC("2017-04-16 09:42:36", "yyyy-MM-dd HH:mm:ss")
  println(date8)
}
