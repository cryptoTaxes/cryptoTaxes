package taxes

import taxes.BaseCurrency._
import taxes.date._
import taxes.io.FileSystem

import spray.json._
import spray.json.JsonProtocol._


object Verbosity extends Enumeration {
  type Level = Int

  val none: Level = 0
  val showDetails: Level = 1
  val showMoreDetails: Level = 2
  val showRates: Level = 3
  val showStocks: Level = 4
  val showAll: Level = 5
}


object PriceCalculation extends Enumeration {
  type Method = Int

  val open: Method = 0
  val close: Method = 1
  val openClose: Method = 2
  val low: Method = 3
  val high: Method = 4
  val lowHigh: Method = 5
}


object Accounting extends Enumeration {
  type Method = Int

  val FIFO: Method = 0
  val LIFO: Method = 1

  def toString(accountingMethod: Method): String =
    accountingMethod match {
      case `FIFO` => "FIFO"
      case `LIFO` => "LIFO"
    }
}

object PriceProvider extends Enumeration {
  type Source = Int

  val CoinMarketCap: Source = 0
  val CoinGecko: Source = 1
}

object Config {
  var config: Config = DefaultConfig

  def verbosity(level: Verbosity.Level): Boolean =
    config.verbosityLevel >= level

  implicit val verbosityJson = jsonEnumFormat(Verbosity)
  implicit val priceCalculationJson = jsonEnumFormat(PriceCalculation)
  implicit val accountingJson = jsonEnumFormat(Accounting)
  implicit val priceProviderJson = jsonEnumFormat(PriceProvider)


  implicit val configJson = jsonFormat14(Config.apply)
}


case class Config(user: String
                  , verbosityLevel: Verbosity.Level
                  , baseCurrency: BaseCurrency
                  , downloadPrices: Boolean
                  , priceProvider: PriceProvider.Source
                  , priceCalculation: PriceCalculation.Method
                  , accountingMethod: Accounting.Method
                  , decimalPlaces: Int
                  , epsilon: Double
                  , timeZone: ZoneId
                  , filterYear: Option[Int]
                  , parityPrioritiesFile: String // in readConfigFolder
                  , fundingFees: Boolean
                  // internal flags, not for users
                  , deprecatedUp2017Version: Boolean
                 ) {

  def saveToFile(path: String): Unit = {
    FileSystem.withPrintStream(path) {
      _.println(this.toJson.prettyPrint)
    }
  }

  def relevantYear(year: Int):Boolean = filterYear match {
    case None =>
      true
    case Some(y) =>
      year == y
  }
}


object DefaultConfig extends Config( user = "demo"
                                   , verbosityLevel = Verbosity.none
                                   , baseCurrency = EuroBaseCurrency
                                   , downloadPrices = false
                                   , priceProvider = PriceProvider.CoinMarketCap
                                   , priceCalculation = PriceCalculation.open
                                   , accountingMethod = Accounting.FIFO
                                   , decimalPlaces = 2
                                   , epsilon = 0.00000001
                                   , timeZone = ZoneId.systemDefault()
                                   , filterYear = None
                                   , parityPrioritiesFile = "parityPriorities.txt"
                                   , fundingFees = false
                                   , deprecatedUp2017Version = false
                                   )

