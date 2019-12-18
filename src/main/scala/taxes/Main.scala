package taxes

import taxes.exchanger.Exchanger
import taxes.priceHistory.{CoinMarketCapPrice, EuroUSDParity}
import taxes.util.Logger
import taxes.util.parse.Parse


object Main extends App {
  // Use English formats for floating point numbers
  java.util.Locale.setDefault(java.util.Locale.ENGLISH)

  val cmdLine = args.mkString(" ")
  Config.config = ParseCommandLine(cmdLine)

  val version = "1.5"

  Logger.trace(s"cryptoTaxes (ver $version) (https://github.com/cryptoTaxes/cryptoTaxes)\n")
  Logger.trace(s"Starting execution at ${java.time.LocalDateTime.now()}.")

  if(Config.config.downloadPrices) {
    CoinMarketCapPrice.downloadPrices()
    EuroUSDParity.downloadPrices()
  }

  // Exchange operation readers
  private val exchangers: List[Exchanger] =
    Exchanger.allExchangers

  // Modules that should be initialized
  private val initializables: List[Initializable] =
    List(TransactionsCache, AddressBook, Filters, TransactionsCache)

  // Do initialization of modules firstly
  for(initializable <- initializables)
    initializable.init()

  Report.process()

  Logger.trace(s"End of execution at ${java.time.LocalDateTime.now()}.")

  // Modules that should be finalized
  private val finalizables = List(TransactionsCache, Logger)

  // Do finalization of modules
  for(finalizable <- finalizables)
    finalizable.wrapUp()
}


object ParseCommandLine {
  def apply(cmdLine: String): Config = {

    var config: Config = DefaultConfig

    var toParse = Parse.trimSpaces(cmdLine)

    val boolTrue = Set("true", "yes", "on")
    val boolFalse = Set("false", "no", "off")

    def toBool(what: String, value: String) = {
      val v = value.toLowerCase
      if(boolFalse.contains(v))
        false
      else if(boolTrue.contains(v))
        true
      else
        Logger.fatal(s"Non valid $what: $value")
    }

    def failParse = Logger.fatal(s"Cannot parse command line: $cmdLine \nFrom here: $toParse")
    while(toParse.nonEmpty) {
      if(toParse.head == '-') {
        toParse = toParse.tail
        val (flag, toParse1) = toParse.span(_ != '=')
        if(flag.nonEmpty && toParse1.nonEmpty && toParse1.head== '=') {
          toParse = toParse1.tail
          val (value, toParse2) = toParse.span(_ != ' ')
          if(value.nonEmpty) {
            toParse = toParse2.dropWhile(_.isSpaceChar)
            if(flag == "user")
              config = config.copy(user = value)
            else if(flag == "verbosity") {
              val level =
                try {
                  Parse.asInt(value)
                } catch {
                  case _ => Logger.fatal(s"Non valid verbosity level: $value")
                }
              config = config.copy(verbosityLevel = level)
            } else if(flag == "currency") {
              val baseCoin = value match {
                case "euro" => EuroBaseCoin
                case "usd"  => USDBaseCoin
                case "btc"  => BTCBaseCoin
                case _      => Logger.fatal(s"Unknown currency: $value")
              }
              config = config.copy(baseCoin = baseCoin)
            } else if(flag == "download-prices") {
              val download = toBool("download prices flag", value)
              config = config.copy(downloadPrices = download)
            } else if(flag == "price-calculation") {
              val method = value match {
                case "open"      => PriceCalculation.open
                case "close"     => PriceCalculation.close
                case "openClose" => PriceCalculation.openClose
                case "low"       => PriceCalculation.low
                case "high"      => PriceCalculation.high
                case "lowHigh"   => PriceCalculation.lowHigh
                case _           => Logger.fatal(s"Unknown price calculation method: $value")
              }
              config = config.copy(priceCalculation = method)
            } else if(flag == "accounting-method") {
              val method = value.toLowerCase match {
                case "fifo"  => Accounting.FIFO
                case "lifo"  => Accounting.LIFO
                case _       => Logger.fatal(s"Unknown accounting method: $value")
              }
              config = config.copy(accountingMethod = method)
            } else if(flag == "decimal-places") {
              val d =
                try {
                  Parse.asInt(value)
                } catch {
                  case _ => Logger.fatal(s"Non valid decimal places: $value")
                }
              config = config.copy(decimalPlaces = d)
            } else if(flag == "epsilon") {
              val eps =
                try {
                  Parse.asDouble(value)
                } catch {
                  case _ => Logger.fatal(s"Non valid epsilon: $value")
                }
              config = config.copy(epsilon = eps)
            } else if(flag == "time-zone") {
              val tz =
                try {
                  date.ZoneId.of(value)
                } catch {
                  case _ => Logger.fatal(s"Non valid time zone $value")
                }
              config = config.copy(timeZone = tz)
            } else if(flag == "deprecated-version") {
              val deprecated = toBool("deprecated version flag", value)
              config = config.copy(deprecatedUp2017Version = deprecated)
            } else if(flag == "year") {
              val year =
                try {
                  Parse.asInt(value)
                } catch {
                  case _ => Logger.fatal(s"Non valid year $value")
                }
              config = config.copy(filterYear = Some(year))
            } else if(flag == "parity-priorities") {
              val fileName = value
              config = config.copy(parityPrioritiesFile = fileName)
            } else
              failParse
          } else
            failParse
        } else
          failParse
      } else
        failParse
    }
    return config
  }
}