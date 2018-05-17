package taxes

import java.util.Calendar

// milliseconds since the standard base time known as "the epoch"
class Date(milliseconds : Long) extends java.util.Date(milliseconds) with Ordered[java.util.Date] {

  // year is ordinary year, unlike java
  // month is in 1-12
  def this(year : Int, month : Int, date : Int) {
    this(new java.util.Date(year-1900, month-1, date).getTime)
  }

  override def getYear: Int =
    super.getYear+1900

  override def getMonth: Int =
    super.getMonth+1

  def at00 : Date =
    Date(this.getYear, this.getMonth, this.getDate)

  override def compare(that: java.util.Date): Int =
    this.getTime.compare(that.getTime)

  override def compareTo(that: java.util.Date): Int =
    this.getTime.compare(that.getTime)

  override def clone(): Date = super.clone().asInstanceOf[Date]

  def sameDayAs(that : Date) : Boolean =
    getDate == that.getDate && getMonth == that.getMonth && getYear == that.getYear

  def nextDay: Date = {
    val calendar = Calendar.getInstance()
    calendar.setTime(this)
    calendar.add(Calendar.DATE, 1) // move onto next day

    return Date(calendar.getTimeInMillis)
  }
}


object Date {
  def apply(milliseconds : Long) =
    new Date(milliseconds)

  def fromUnix(seconds : Long) =
    new Date(seconds * 1000L)

  // month is in 1-12
  def apply(year : Int, month : Int, date : Int) =
    new Date(year, month, date)

  implicit val ord : Ordering[Date] = Ordering.by(_.getTime)

  def fromString(str : String, format : String) : Date = {
    import java.text.SimpleDateFormat
    val fmt = new SimpleDateFormat(format, java.util.Locale.ENGLISH)
    val javaDate = fmt.parse(str)
    new Date(javaDate.getTime)
  }

  def fromOffsetString(str : String) : Date = {
    val offsetDateTime = java.time.OffsetDateTime.parse(str)
    new Date(offsetDateTime.toEpochSecond*1000)
  }
}
