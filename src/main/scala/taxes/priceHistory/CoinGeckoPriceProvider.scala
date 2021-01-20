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

  // private val currenciesToIndexes = Map("BTC" -> 1, "ETH" -> 279, "ZEC" -> 486, "USDT" -> 325)

  private lazy val allPairs = Parse.readKeyValues(
    FileSystem.inConfigFolder("coingeckoCurrencies.txt")
    , "Reading coingecko currencies").map{
    case (currency, iterable) =>
      iterable.toSeq match {
        case Seq(url, idx) =>
          (Currency.normalize(currency), (url, Parse.asInt(idx)))
      }
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

  private def downloadPricesFor(currency: Currency, coinGeckoIdx: Int): List[DailyPrice] = {
    Logger.trace(s"Downloading prices for $currency from coingecko.com.")
    val (yearBegin, yearEnd) = Config.config.yearRange()

    val url = s"https://www.coingecko.com/price_charts/export/$coinGeckoIdx/usd.csv"
    return Network.Http.withSource(url){ src =>
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

    val ext = Config.config.filterYear match {
      case None => FileSystem.coinGeckoExtension
      case Some(year) => FileSystem.coinGeckoExtension(year)
    }

    val path = FileSystem.coinGeckoFolder(currency)

    val src = new FolderSource[DailyPrice](path, ext) {
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
    for((currency, (coinGeckoId, coinGeckoIdx)) <- allPairs) {
      Thread.sleep(500) // to avoid Http 429 error
      val dailyPrices = downloadPricesFor(currency, coinGeckoIdx)
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
    for((currency, (coinGeckoId, coinGeckoIdx)) <- allPairs)
      yield (currency, CoinGeckoPrice(currency))
  }

  // returns price in USD for a currency at a given date
  def priceInUSD(currency: Currency, date: LocalDateTime): Price =
    currencies2CoinGeckoPrices.get(currency) match  {
      case Some(coinGeckoPrice) => coinGeckoPrice(date)
      case None => Logger.fatal(s"prices for $currency not found.")
    }
}