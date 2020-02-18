package taxes.priceHistory

import taxes._
import taxes.date._
import taxes.io.{FileSystem, Network}
import taxes.util._
import taxes.util.parse.{Parse, SeparatedScanner}

import scala.collection.mutable.ListBuffer

import spray.json._
import spray.json.JsonProtocol._

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
    if(!line.contains(token1)) {
      Logger.fatal(s"Error reading coinmarketcap prices: $line.")
    }
    val skipBeginning = line.dropWhile(_ != token1).tail

    if(!line.contains(token2)) {
      Logger.fatal(s"Error reading coinmarketcap prices: $line.")
    }
    val contents = skipBeginning.takeWhile(_ != token2)
    return contents
  }

  private def downloadHtmlFor(currency: Currency, coinMarketCapId: String) {
    for(year <- 2013 to LocalDateTime.now.getYear) {
      val file = io.FileSystem.File(s"data/html/$currency.$year.html")
      if(!file.exists()) {
        Thread.sleep(5000) // to avoid Http 429 error
        scala.util.Try{
          val url = s"https://coinmarketcap.com/currencies/$coinMarketCapId/historical-data/?start=${year}0101&end=${year}1231"
          Network.Http.withSource(url) { src =>
            io.FileSystem.withPrintStream(file) { ps =>
              ps.print(src.mkString)
            }
          }
        }
      }
    }
  }

  private def downloadPricesFor(currency: Currency, coinMarketCapId: String): List[DailyPrice] = {
    Logger.trace(s"Downloading prices for $currency from coinmarketcap.com.")
    val (yearBegin, yearEnd) = Config.config.filterYear match {
      case None =>
        import java.util.Calendar
        val thisYear = Calendar.getInstance.get(Calendar.YEAR)
        (2010, thisYear)
      case Some(year) => (year, year)
    }

    val url = s"https://coinmarketcap.com/currencies/$coinMarketCapId/historical-data/?start=${yearBegin}0101&end=${yearEnd}1231"
    return Network.Http.withSource(url){ src =>
      scrapPrices(currency, coinMarketCapId, src)
    }
  }

 private def scrapPrices(currency: Currency, coinMarketCapId: String, source: FileSystem.Source): List[DailyPrice] = {
   val tokenBeginRow = "<td class=\"cmc-table__cell cmc-table__cell--sticky cmc-table__cell--left\"><div class=\"\">"
   val tokenBeginPrice = "<td class=\"cmc-table__cell cmc-table__cell--right\"><div class=\"\">"
   val tokenEnd = "<"

   var textLine = source.getLines().dropWhile(_.indexOf(tokenBeginRow) < 0).next()

   def getToken(before: String, after: String): String = {
     var idx = textLine.indexOf(before)
     if(idx < 0)
       Logger.fatal(s"Error reading coinmarketcap prices. Couldn't find $before.")

     textLine = textLine.drop(idx+before.length)

     idx = textLine.indexOf(after)
     if(idx < 0)
       Logger.fatal(s"Error reading coinmarketcap prices. Couldn't find $after.")

     val token = textLine.take(idx)
     textLine = textLine.drop(idx+after.length)
     return token
   }

   def getPrice(): Double = Parse.asDouble(getToken(tokenBeginPrice, tokenEnd))

   val dailyPrices = ListBuffer[DailyPrice]()

   var goOn = true
   while(goOn) {
     val idx = textLine.indexOf(tokenBeginRow)
     if(idx < 0)
       goOn = false
     else {
       val dateStr = getToken(tokenBeginRow, tokenEnd)

       val sc = SeparatedScanner(dateStr, "[\t ,]+")
       val month = sc.next()
       val day = sc.nextInt()
       val year = sc.nextInt()
       sc.close()

       val date = LocalDate.apply(year, parseMonth(month), day)
       val open = getPrice()
       val high = getPrice()
       val low = getPrice()
       val close = getPrice()

       val dailyPrice = DailyPrice(date, open, high, low, close)
       dailyPrices += dailyPrice
     }
   }

   if(dailyPrices.isEmpty)
     Logger.fatal(s"Error reading coinmarketcap prices. Couldn't find $tokenBeginRow.")

   return dailyPrices.toList
  }

  private def scrapPricesOld(currency: Currency, coinMarketCapId: String, source: FileSystem.Source): List[DailyPrice] = {
    val tokenBegin = "<table class=\"table\">"
    val tokenEnd = "</tbody>"
    val tokenNoResults = "<tr class=\"text-center\">"

    val inLines = source.getLines.filter(_.nonEmpty).map(_.dropWhile(_.isSpaceChar)).dropWhile(_ != tokenBegin).drop(13)
    if(inLines.isEmpty)
      Logger.fatal(s"Error reading coinmarketcap prices. Couldn't find $tokenBegin.")

    val dailyPrices = ListBuffer[DailyPrice]()

    var goOn = true
    while(goOn) {
      val line1 = inLines.next()
      if(line1 == "<tr class=\"text-right\">") {
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
        for(_ <- 0 until 3)
          inLines.next()

        val dailyPrice = DailyPrice(date, open, high, low, close)
        dailyPrices += dailyPrice
      } else {
        goOn = false
        if(line1 != tokenEnd && line1 != tokenNoResults)
          Logger.fatal(s"Something went wrong scrapping prices for $coinMarketCapId.\n$line1")
      }
    }
    return dailyPrices.toList
  }

  private def saveToDisk(currency: Currency, dailyPrices: List[DailyPrice]): Unit = {
    def relevant(year: Int):Boolean = Config.config.filterYear match {
      case None =>
        true
      case Some(y) =>
        year == y
    }

    val map = dailyPrices.groupBy(_.date.getYear)
    for((year, dailyPrices) <- map)
      if(relevant(year)){
        val fileName = FileSystem.coinMarketCapFile(currency, year)
        FileSystem.withPrintStream(fileName) { ps =>
          ps.print(dailyPrices.toJson.prettyPrint)
        }
      }
  }

   private lazy val allPairs = Parse.readKeysValue(
    FileSystem.inConfigFolder("coinmarketcapCurrencies.txt")
    , "Reading coinmarketcap currencies").map{
      case (currency, url) => (Currency.normalize(currency), url)
    }

  def downloadPrices(): Unit = {
    for((currency, coinMarketCapId) <- allPairs) {
      Thread.sleep(5000) // to avoid Http 429 error
      val dailyPrices = downloadPricesFor(currency, coinMarketCapId)
      saveToDisk(currency, dailyPrices)
    }
  }

  private def loadFromDisk(currency: Currency): scala.collection.mutable.Map[LocalDate, Price] = {
    Logger.trace(s"Loading CoinMarketCap prices for $currency.")

    val ext = Config.config.filterYear match {
      case None => FileSystem.coinMarketCapExtension
      case Some(year) => FileSystem.coinMarketCapExtension(year)
    }

    val path = FileSystem.coinMarketCapFolder(currency)

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
        case PriceCalculation.close     => dailyPrice.close
        case PriceCalculation.openClose => (dailyPrice.open + dailyPrice.close) / 2
        case PriceCalculation.low       => dailyPrice.low
        case PriceCalculation.high      => dailyPrice.high
        case PriceCalculation.lowHigh   => (dailyPrice.high + dailyPrice.low) / 2
      }

    return map
  }

  case class CoinMarketCapPrice(currency: Currency) extends PriceHistory {
    lazy val prices = loadFromDisk(currency)

    def apply(date: LocalDateTime): Price =
      prices.get(LocalDate.of(date)) match {
        case None =>
          Logger.fatal(s"price for $currency at $date not found")
        case Some(price) => price
      }
  }

  private lazy val currencies2CoinMarketCapPrices = {
    for((currency, _) <- allPairs)
      yield (currency, CoinMarketCapPrice(currency))
  }

  // returns price in USD for a currency at a given date
  def apply(currency: Currency, date: LocalDateTime): Price =
    currencies2CoinMarketCapPrices.get(currency) match  {
      case Some(coinMarketCapPrice) => coinMarketCapPrice(date)
      case None => Logger.fatal(s"prices for $currency not found.")
    }
}

