package taxes

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
}


object Date {
  def apply(unixTime : Long) =
    new Date(unixTime)

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
