package taxes.priceHistory

import spray.json._
import spray.json.JsonProtocol._
import taxes.date._
import taxes.io.FileSystem.Source
import taxes.io.{FileSystem, Network}
import taxes.{Config, Currency, FileSource, FolderSource, Price, PriceCalculation}
import taxes.util.Logger
import taxes.util.parse.Parse

import scala.collection.mutable.ListBuffer

object CoinGeckoPriceProvider extends CryptoPriceProvider {
  override def toString: Currency = "CoinGecko"

  case class DailyPrice(date: LocalDate, open: Double)
  implicit val dailyPriceJson = jsonFormat2(DailyPrice)

  private lazy val allPairs = Parse.readKeysValue(
    FileSystem.inConfigFolder("coingeckoCurrencies.txt")
    , "Reading coingecko currencies").map{
      case (currency, coinGeckoId) =>
          (Currency.normalize(currency), coinGeckoId)
    }


  private def scrapPrices(currency: taxes.Currency, source: Source): List[DailyPrice] = {
    val lines = source.getLines().drop(1) // skip header
    val dailyPrices = ListBuffer[DailyPrice]()
    for(line <- lines) {
      if(line.contains(",,"))
        Logger.warning(s"CoinGecko price for $currency may be wrong $line")
      val dateStr::priceStr::_ = Parse.sepBy(line, ",")
      val year::month::day::_ = Parse.sepBy(dateStr, "-")
      val date = LocalDate.apply(Parse.asInt(year), Parse.asInt(month), Parse.asInt(day))
      val open = Parse.asDouble(priceStr)
      dailyPrices += DailyPrice(date, open)
    }

    if(dailyPrices.isEmpty)
      Logger.fatal(s"Error reading coinGecko prices. Couldn't parse csv file.")

    return dailyPrices.toList
  }

  private def downloadPricesForOLD(currency: Currency, coinGeckoIdx: Int): List[DailyPrice] = {
    Logger.trace(s"Downloading prices for $currency from coingecko.com.")

    val url = s"https://www.coingecko.com/price_charts/export/$coinGeckoIdx/usd.csv"
    return Network.Http.withSource(url){ src =>
      scrapPrices(currency, src)
    }
  }

  private def downloadPricesFor(currency: Currency, coinGeckoId: String): List[DailyPrice] = {
    Logger.trace(s"Downloading prices for $currency from coingecko.com.")

    val suffix = "/usd.csv"
    val historicalDataUrl = s"https://www.coingecko.com/coins/$coinGeckoId/historical_data/usd"

    val usdCsvUrl = Network.Http.withSource(historicalDataUrl) { src =>
      val matchingLines = src.getLines().filter(_.contains(suffix))
      if(matchingLines.isEmpty)
        Logger.fatal(s"CoinGeckoProvider.downloadPricesFor $currency $coinGeckoId. Cannot find csv file.")
      else {
        val line = matchingLines.next()

        val tkBefore = "href="
        val delimiter = "\""
        val Some(str) = Parse.skipUntil(line, tkBefore)
        val Some(fullSuffix) = Parse.unquote(str, delimiter)
        s"https://www.coingecko.com$fullSuffix"
      }
    }
    return Network.Http.withSourceAndConnection(usdCsvUrl){ (src, conn) =>
      val fieldValue = conn.getHeaderField("Content-Disposition")
      val Some(str) = Parse.skipUntil(fieldValue, "filename=")
      val Some(fileName) = Parse.unquote(str, "\"")
      Logger.trace(s"Downloading file $fileName.")
      val fileCurrency = fileName.takeWhile(_ != '-')
      if(Currency.normalize(fileCurrency) != currency)
        Logger.warning(s"CoinGeckoPriceProvider.downloadPricesFor: looks like downloaded price file ($fileName) doesn't match $currency")

      scrapPrices(currency, src)
    }
  }

  private def saveToDisk(currency: Currency, dailyPrices: List[DailyPrice]): Unit = {
    val map = dailyPrices.groupBy(_.date.getYear)
    for((year, dailyPrices) <- map)
      if(Config.config.relevantYear(year)){
        val fileName = FileSystem.coinGeckoFile(currency, year)
        FileSystem.withPrintStream(fileName) { ps =>
          ps.print(dailyPrices.toJson.prettyPrint)
        }
      }
  }

  private def loadFromDisk(currency: Currency): scala.collection.mutable.Map[LocalDate, Price] = {
    Logger.trace(s"Loading CoinGecko prices for $currency.")

    val path = FileSystem.coinGeckoFolder(currency)

    val src = new FolderSource[DailyPrice](path, FileSystem.coinGeckoExtension, Config.config.filterYear) {
      override def fileSource(fileName: String): FileSource[DailyPrice] =
        new FileSource[DailyPrice](fileName) {
          override def read(): Seq[DailyPrice] =
            FileSystem.withSource(fileName) { src =>
              src.mkString.parseJson.convertTo[Seq[DailyPrice]]
            }
        }
    }

    val map = scala.collection.mutable.Map[LocalDate, Price]()
    for(dailyPrice <- src.read())
      map(dailyPrice.date) = Config.config.priceCalculation match {
        case PriceCalculation.open      => dailyPrice.open
        case _                          => Logger.fatal(s"CoinGecko only supports opening prices")
      }

    return map
  }
  def downloadPrices(): Unit = {
    for((currency, coinGeckoId) <- allPairs) {
      Thread.sleep(500) // to avoid Http 429 error
      val dailyPrices = downloadPricesFor(currency, coinGeckoId)
      saveToDisk(currency, dailyPrices)
    }
  }


  case class CoinGeckoPrice(currency: Currency) extends PriceHistory {
    lazy val prices = loadFromDisk(currency)

    def apply(date: LocalDateTime): Price =
      prices.get(LocalDate.of(date)) match {
        case None =>
          Logger.fatal(s"price for $currency at $date not found")
        case Some(price) => price
      }
  }

  private lazy val currencies2CoinGeckoPrices = {
    for((currency, coinGeckoId) <- allPairs)
      yield (currency, CoinGeckoPrice(currency))
  }

  // returns price in USD for a currency at a given date
  def priceInUSD(currency: Currency, date: LocalDateTime): Price =
    currencies2CoinGeckoPrices.get(currency) match  {
      case Some(coinGeckoPrice) => coinGeckoPrice(date)
      case None => Logger.fatal(s"prices for $currency not found.")
    }
}