package taxes.date

object LocalDate {
  def apply(year : Int, month : Int, dayOfMonth : Int) : LocalDate =
    java.time.LocalDate.of(year, month, dayOfMonth)

  def of(localDateTime : LocalDateTime) : LocalDate =
    localDateTime.toLocalDate
}