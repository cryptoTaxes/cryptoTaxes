package taxes

import taxes.util.Logger

object Filters extends Initializable {

  trait What
  case object All extends What
  case class Currency(currency: taxes.Currency) extends What

  trait Filter { val year: Int }
  case class Stock(year: Int, what: What) extends Filter
  case class Exchanger(year: Int, exchanger: taxes.exchanger.Exchanger, what: What) extends Filter
  case class Margin(year: Int, exchanger: taxes.exchanger.Exchanger, what: What) extends Filter

  val allToken = "*"
  val sepToken = ":"
  val stocksToken = "stocks"
  val marginToken = "margin"

  private def parseWhat(str: String): What =
    if(str==allToken)
      All
    else
      Currency(taxes.Currency.normalize(str))

  private def parseLine(left: String, rights: Iterable[taxes.Currency]): Iterable[Filter] = {
    import util.parse.Parse._

    val tokens = sepBy(left, sepToken).map(trimSpaces)
    tokens match {
      case List(str) =>
        val year = asInt(str)
        rights.map(str => Stock(year, parseWhat(str)))
      case List(str1, str2) =>
        val year = asInt(str1)

        if(str2==stocksToken) {
          rights.map(str => Stock(year, parseWhat(str)))
        } else {
          val exchanger = taxes.exchanger.Exchanger.parse(str2)
          rights.map(str => Exchanger(year, exchanger, parseWhat(str)))
        }
      case List(str1, str2, str3) =>
        val year = asInt(str1)
        val exchanger = taxes.exchanger.Exchanger.parse(str2)
        val margin = str3
        if(margin != marginToken)
          Logger.fatal(s"Error parsing file ${io.FileSystem.filtersFile}: 'margin' expected but found $margin.")
        rights.map(str => Margin(year, exchanger, parseWhat(str)))

      case _ =>
        Logger.fatal(s"Error parsing file ${io.FileSystem.filtersFile}: 'margin' expected  $left.")
    }
  }

  private val filters: Map[Int, Iterable[Filter]] = // from year to filters
    util.parse.Parse.readKeyValues(io.FileSystem.filtersFile, "Reading filters list.").flatMap{
      case (what, currencies) => parseLine(what, currencies)
    }.groupBy(_.year)

  def applyFilters(year: Int, allStocks: StockPool): Unit = {
    for(filter <- filters.getOrElse(year, Seq())) {
      filter match {
        case Stock(year, what) =>
          val currencies = what match {
            case All =>
              allStocks.map(_.currency)
            case Currency(currency) =>
              List(currency)
          }
          for(currency <- currencies)
            allStocks.delete(currency)
          for(exchanger <- taxes.exchanger.Exchanger.allExchangers) {
            for(currency <- currencies)
              exchanger.ledgerPool.delete(currency)
          }

        case Exchanger(year, exchanger, what) =>
          val currencies = what match {
            case All =>
              exchanger.ledgerPool.map(_.currency)
            case Currency(currency) =>
              List(currency)
          }
          for(currency <- currencies)
            exchanger.ledgerPool.delete(currency)

        case Margin(year, exchanger, what) =>
          val ids = what match {
            case All =>
              exchanger.marginLongs.map(_.id) ++
                exchanger.marginShorts.map(_.id)
            case Currency(id) =>
              List(id)
          }
          for(id <- ids) {
            exchanger.marginLongs.delete(id)
            exchanger.marginShorts.delete(id)
          }
      }
    }
  }
}
