package taxes


object Verbosity extends Enumeration {
  type Level = Int

  val none : Int = 0
  val showDetails : Int = 1
  val showMoreDetails : Int = 2
  val showRates : Int = 3
  val showStocks : Int = 4
}


case class Config( user : String
                  , verbosityLevel : Verbosity.Level
                  , baseCoin : BaseCoin
                  , downloadPrices : Boolean
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
                                     )

