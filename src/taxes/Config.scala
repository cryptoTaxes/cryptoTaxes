package taxes


object Verbosity extends Enumeration {
  type Level = Int

  val none : Level = 0
  val showDetails : Level = 1
  val showMoreDetails : Level = 2
  val showRates : Level = 3
  val showStocks : Level = 4
}


object PriceCalculation extends Enumeration {
  type Method = Int

  val open : Method = 0
  val close : Method = 1
  val openClose : Method = 2
  val high : Method = 3
  val low : Method = 4
  val lowHigh : Method = 5
}


case class Config( user : String
                  , verbosityLevel : Verbosity.Level
                  , baseCoin : BaseCoin
                  , downloadPrices : Boolean
                  , priceCalculation: PriceCalculation.Method
                  )


object Config {
  var config : Config = DefaultConfig

  def verbosity(level : Verbosity.Level) : Boolean =
    config.verbosityLevel >= level
}


object DefaultConfig extends Config( user = "demo"
                                     , verbosityLevel = Verbosity.none
                                     , baseCoin = EuroBaseCoin
                                     , downloadPrices = false
                                     , priceCalculation = PriceCalculation.open
                                     )

