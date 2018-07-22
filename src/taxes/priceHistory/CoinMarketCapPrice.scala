package taxes.priceHistory

import taxes._
import taxes.date._
import taxes.io.{FileSystem, Network}
import taxes.util._
import taxes.util.parse.{Parse, SeparatedScanner}

import scala.collection.mutable.ListBuffer

import spray.json._
import DefaultJsonProtocol._

object CoinMarketCapPrice {
  case class DailyPrice(date: LocalDate, open: Double, high: Double, low: Double, close: Double)
  implicit val dailyPriceJson = jsonFormat5(DailyPrice)

  private def parseMonth(month: String): Int =
    month match {
      case "Jan" => 1
      case "Feb" => 2
      case "Mar" => 3
      case "Apr" => 4
      case "May" => 5
      case "Jun" => 6
      case "Jul" => 7
      case "Aug" => 8
      case "Sep" => 9
      case "Oct" => 10
      case "Nov" => 11
      case "Dec" => 12
    }

  private def scrapContents(line: String): String = {
    val token1 = '>'
    val token2 = '<'
    if (!line.contains(token1)) {
      Logger.fatal(s"Error reading coinmarketcap prices: $line.")
    }
    val skipBeginning = line.dropWhile(_ != token1).tail

    if (!line.contains(token2)) {
      Logger.fatal(s"Error reading coinmarketcap prices: $line.")
    }
    val contents = skipBeginning.takeWhile(_ != token2)
    return contents
  }

  private def downloadPricesFor(market: Market, coinMarketCapID: String): List[DailyPrice] = {
    Logger.trace(s"Downloading prices for $market from coinmarketcap.com.")
    val (yearBegin, yearEnd) = Config.config.filterYear match {
      case None => (2010, 2500)
      case Some(year) => (year, year)
    }

    val url = s"https://coinmarketcap.com/currencies/$coinMarketCapID/historical-data/?start=${yearBegin}0101&end=${yearEnd}1231"
    val contents = Network.fromHttpURL(url)
    return scrapPrices(market, coinMarketCapID, contents)
  }

  private def scrapPrices(market: Market, coinMarketCapID: String, contents: String): List[DailyPrice] = {
    val tokenBegin = "<table class=\"table\">"
    val tokenEnd = "</tbody>"
    val tokenNoResults = "<tr class=\"text-center\">"

    val inLines = contents.split("\n").filter(_.nonEmpty).map(_.dropWhile(_.isSpaceChar)).dropWhile(_ != tokenBegin).drop(13).iterator
    if (inLines.isEmpty)
      Logger.fatal(s"Error reading coinmarketcap prices. Couldn't find $tokenBegin.")

    val dailyPrices = ListBuffer[DailyPrice]()

    var goOn = true
    while (goOn) {
      val line1 = inLines.next()
      if (line1 == "<tr class=\"text-right\">") {
        val dateStr = scrapContents(inLines.next())

        val sc = SeparatedScanner(dateStr, "[\t ,]+")
        val month = sc.next()
        val day = sc.nextInt()
        val year = sc.nextInt()
        sc.close()

        val date = LocalDate.apply(year, parseMonth(month), day)
        val open = Parse.asDouble(scrapContents(inLines.next()))
        val high = Parse.asDouble(scrapContents(inLines.next()))
        val low = Parse.asDouble(scrapContents(inLines.next()))
        val close = Parse.asDouble(scrapContents(inLines.next()))

        // skip 4 lines
        for (_ <- 0 until 3)
          inLines.next()

        val dailyPrice = DailyPrice(date, open, high, low, close)
        dailyPrices += dailyPrice
      } else {
        goOn = false
        if (line1 != tokenEnd && line1 != tokenNoResults)
          Logger.fatal(s"Something went wrong scrapping prices for $coinMarketCapID.\n$line1")
      }
    }
    return dailyPrices.toList
  }

  private def saveToDisk(market: Market, dailyPrices: List[DailyPrice]): Unit = {
    val map = dailyPrices.groupBy(_.date.getYear)

    for ((year, dailyPrices) <- map) {
      val fileName = FileSystem.coinMarketCapFile(market, year)
      FileSystem.withPrintStream(fileName) { ps =>
        ps.print(dailyPrices.toJson.prettyPrint)
      }
    }
  }

  private def parsePair(t : (Market, String)) = t match {
    case (market1, market2) => (Market.normalize(market1), market2.toLowerCase())
  }

  private lazy val allPairs = Parse.readAssociations (
    FileSystem.configFile("coinmarketcapMarkets.txt")
    , "Reading coinmarketcap markets"
  ).map(parsePair)


  def downloadPrices(): Unit = {
    for((market, coinMarketCapID) <- allPairs) {
      val dailyPrices = downloadPricesFor(market, coinMarketCapID)
      saveToDisk(market, dailyPrices)
    }
  }

  private def loadFromDisk(market: Market): scala.collection.mutable.Map[LocalDate, Price] = {
    Logger.trace(s"Loading CoinMarketCap prices for $market.")

    val ext = Config.config.filterYear match {
      case None => FileSystem.coinMarketCapExtension
      case Some(year) => s"$year${FileSystem.coinMarketCapExtension}"
    }

    val path = FileSystem.coinMarketCapFolder(market)

    val src = new FolderSource[DailyPrice](path, ext) {
      override def fileSource(fileName: String): FileSource[DailyPrice] =
        new FileSource[DailyPrice](fileName) {
          override def read(): Seq[DailyPrice] =
            FileSystem.withSource(fileName) { src =>
              JsonParser(src.mkString).convertTo[Seq[DailyPrice]]
            }
        }
    }

    val map = scala.collection.mutable.Map[LocalDate, Price]()
    for (dailyPrice <- src.read())
      map(dailyPrice.date) = Config.config.priceCalculation match {
        case PriceCalculation.open      => dailyPrice.open
        case PriceCalculation.close     => dailyPrice.close
        case PriceCalculation.openClose => (dailyPrice.open + dailyPrice.close) / 2
        case PriceCalculation.high      => dailyPrice.high
        case PriceCalculation.low       => dailyPrice.low
        case PriceCalculation.lowHigh   => (dailyPrice.high + dailyPrice.low) / 2
      }

    return map
  }

  case class CoinMarketCapPrice(market : Market) extends PriceHistory {
    lazy val prices = loadFromDisk(market)

    def apply(date : LocalDateTime) : Price =
      prices.get(LocalDate.of(date)) match {
        case None =>
          Logger.fatal(s"price for $market at $date not found")
        case Some(price) => price
      }
  }

  private lazy val markets2CoinMarketPrices = {
    for((market, _) <- allPairs)
      yield (market, CoinMarketCapPrice(market))
  }

  // returns price in USD for a market at a given date
  def apply(market : Market, date : LocalDateTime) : Price =
    markets2CoinMarketPrices.get(market) match  {
      case Some(coinMarketCapPrice) => coinMarketCapPrice(date)
      case None => Logger.fatal(s"price for $market at $date not found.")
    }
}

