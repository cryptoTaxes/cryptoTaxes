package taxes

import taxes.exchanger.Exchanger
import taxes.priceHistory.{CoinMarketCapPrice, EuroUSDParity}
import taxes.util.Logger
import taxes.util.parse.Parse


object Main extends App {
  // Use English formats for floating point numbers
  java.util.Locale.setDefault(java.util.Locale.ENGLISH)

  val cmdLine = args.mkString(" ")
  //val cmdLine = "-user=user1 -verbosity=3 -currency=euro -download-prices=no"
  Config.config = ParseCommandLine(cmdLine)

  Logger.trace("cryptoTaxes (https://github.com/cryptoTaxes/cryptoTaxes)\n")

  if(Config.config.downloadPrices) {
    CoinMarketCapPrice.downloadPrices()
    EuroUSDParity.downloadPrices()
  }

  // Exchange operation readers
  private val exchangers : List[Exchanger] =
    Exchanger.allExchangers

  // Modules that should be initalized
  private val initializables : List[Initializable] =
    List(FileSystem, Market, CoinMarketCapPrice, EuroUSDParity, Logger, TransactionsCache) ++ exchangers

  // Do initialization of modules firstly
  for(initializable <- initializables)
    initializable.init()

  private val nets = Report.process()

  // Modules that should be finalize
  private val finalizables = List(Logger, TransactionsCache)

  // Do finalization of modules
  for(finalizable <- finalizables)
    finalizable.wrapUp()
}


object ParseCommandLine {
  def apply(cmdLine : String) : Config = {

    var config : Config = DefaultConfig

    var toParse = Parse.trimSpaces(cmdLine)

    def failParse = Logger.fatal(s"Cannot parse command line: $cmdLine \nFrom here: $toParse")
    while (toParse.nonEmpty) {
      if (toParse.head == '-') {
        toParse = toParse.tail
        val (flag, toParse1) = toParse.span(_ != '=')
        if(flag.nonEmpty && toParse1.nonEmpty && toParse1.head== '=') {
          toParse = toParse1.tail
          val (value, toParse2) = toParse.span(_ != ' ')
          if(value.nonEmpty) {
            toParse = toParse2.dropWhile(_.isSpaceChar)
            if (flag == "user")
              config = config.copy(user = value)
            else if (flag == "verbosity") {
              val level =
                try {
                  Parse.asInt(value)
                } catch {
                  case e: Exception => Logger.fatal(s"Non valid verbosity level: $value")
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
              val download = List("true", "yes", "on").contains(value.toLowerCase)
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
              val method = value match {
                case "fifo" | "FIFO"  => Accounting.FIFO
                case "lifo" | "LIFO"  => Accounting.LIFO
                case _                => Logger.fatal(s"Unknown accounting method: $value")
              }
              config = config.copy(accountingMethod = method)
            } else if(flag == "decimal-places") {
              val d =
                try {
                  Parse.asInt(value)
                } catch {
                  case e: Exception => Logger.fatal(s"Non valid decimal places: $value")
                }
              config = config.copy(decimalPlaces = d)
            } else if(flag == "epsilon") {
              val eps =
                try {
                  Parse.asDouble(value)
                } catch {
                  case e: Exception => Logger.fatal(s"Non valid epsilon: $value")
                }
              config = config.copy(epsilon = eps)
            } else if(flag == "time-zone") {
              config = config.copy(timeZone = java.time.ZoneId.of(value))
            } else if(flag == "deprecated-version") {
              val old = List("true", "yes", "on").contains(value.toLowerCase)
              config = config.copy(deprecatedUp2017Version = old)
            } else if(flag == "year") {
              val year = Parse.asInt(value)
              config = config.copy(filterYear = Some(year))
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