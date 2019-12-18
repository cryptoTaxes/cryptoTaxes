package taxes

import taxes.util.Logger

object Filters extends Initializable {

  trait What
  case object All extends What
  case class Market(market: taxes.Market) extends What

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
      Market(taxes.Market.normalize(str))

  private def parseLine(left: String, rights: Iterable[taxes.Market]): Iterable[Filter] = {
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
      case (what, markets) => parseLine(what, markets)
    }.groupBy(_.year)

  def applyFilters(year: Int, allStocks: StockPool): Unit = {
    for(filter <- filters.getOrElse(year, Seq())) {
      filter match {
        case Stock(year, what) =>
          val markets = what match {
            case All =>
              allStocks.map(_.market)
            case Market(market) =>
              List(market)
          }
          for(market <- markets)
            allStocks.delete(market)
          for(exchanger <- taxes.exchanger.Exchanger.allExchangers) {
            for(market <- markets)
              exchanger.ledgerPool.delete(market)
          }

        case Exchanger(year, exchanger, what) =>
          val markets = what match {
            case All =>
              exchanger.ledgerPool.map(_.market)
            case Market(market) =>
              List(market)
          }
          for(market <- markets)
            exchanger.ledgerPool.delete(market)

        case Margin(year, exchanger, what) =>
          val ids = what match {
            case All =>
              exchanger.marginLongs.map(_.id) ++
                exchanger.marginShorts.map(_.id)
            case Market(market) =>
              List(market)
          }
          for(id <- ids) {
            exchanger.marginLongs.delete(id)
            exchanger.marginShorts.delete(id)
          }
      }
    }
  }
}
