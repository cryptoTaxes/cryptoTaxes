package taxes.priceHistory

import taxes._
import taxes.date._
import taxes.util._
import taxes.util.parse.Parse

import scala.collection.mutable.ListBuffer
import scala.io.Source


object CoinMarketCapPrice extends Initializable {
  def apply(market : Market, fileFullPath : String, coinMarketCapID: String) : CoinMarketCapPrice =
    new CoinMarketCapPrice(market, fileFullPath, coinMarketCapID)

  private def parseMonth(month : String) : Int =
    month match {
      case "Jan" =>  1
      case "Feb" =>  2
      case "Mar" =>  3
      case "Apr" =>  4
      case "May" =>  5
      case "Jun" =>  6
      case "Jul" =>  7
      case "Aug" =>  8
      case "Sep" =>  9
      case "Oct" => 10
      case "Nov" => 11
      case "Dec" => 12
    }

  private def parsePair(t : (Market, String)) = t match {
    case (market1, market2) => (Market.normalize(market1), market2.toLowerCase())
  }

  private val allPairs = Parse.readAssociations (
    FileSystem.configFile("coinmarketcapMarkets.txt")
    , "Reading coinmarketcap markets"
    ).map(parsePair)


  private val markets2CoinMarketPrices = {
    for((market, id) <- allPairs)
      yield (market, CoinMarketCapPrice(market, s"${FileSystem.coinMarketCap}/${market.toLowerCase}.txt", id))
  }

  // returns price in USD for a market at a given date
  def apply(market : Market, date : LocalDateTime) : Price =
    markets2CoinMarketPrices.get(market) match  {
      case Some(prices) => prices(date)
      case None => Logger.fatal(s"price for $market at $date not found")
    }

  private def scrapContents(line : String) : String = {
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

  private def scrapPrices(coinMarketCapID: String) : Seq[String] = {
    val url = s"https://coinmarketcap.com/currencies/$coinMarketCapID/historical-data/?start=20130101&end=20500101"
    val src = Source.fromURL(url)
    /*
    val src = Source.fromFile(coinMarketCapID)
    Thread.sleep(5000)
    val ps = FileSystem.PrintStream(coinMarketCapID)
    ps.println(src.mkString)
    ps.close()
    */
    val tokenBegin = "<table class=\"table\">"
    val tokenEnd = "</tbody>"

    val inLines = src.getLines().filter(_.nonEmpty).map(_.dropWhile(_.isSpaceChar)).dropWhile( _ != tokenBegin).drop(13)
    if(inLines.isEmpty)
      Logger.fatal(s"Error reading coinmarketcap prices. Couldn't find $tokenBegin.")

    val outLines = ListBuffer[String]()

    var goOn = true
    while(goOn) {
      val line1 = inLines.next()
      //println("xx"+line1+"xx")
      if(line1=="<tr class=\"text-right\">") {
        val date = scrapContents(inLines.next())
        val open = scrapContents(inLines.next())
        val high = scrapContents(inLines.next())
        val low = scrapContents(inLines.next())
        val close = scrapContents(inLines.next())

        // skip 4 lines
        for(i <- 0 until 3)
          inLines.next()

        outLines += List(date,open,high,low,close).mkString("\t")
      } else {
        goOn = false
        if(line1 != tokenEnd)
          Logger.fatal(s"Something went wrong scrapping prices for $coinMarketCapID.")
      }
    }
    src.close()
    return outLines
  }

  def downloadPrices(): Unit = {
    for(coinmarketcap <- markets2CoinMarketPrices.values)
      coinmarketcap.downloadPrices()
  }

}

class CoinMarketCapPrice(market : Market, fileFullPath : String, coinMarketCapID: String) extends PriceHistory {

  private def readPrices() : scala.collection.mutable.Map[LocalDate, Price] = {
    Logger.trace(s"Reading CoinMarketPrice from $fileFullPath.")
    val prices = scala.collection.mutable.Map[LocalDate, Price]()
    val file = new java.io.File(fileFullPath)
    val sc = new java.util.Scanner(file)
    val header = sc.nextLine()
    var lineNumber = 0
    while (sc.hasNextLine) {
      val line = sc.nextLine()
      lineNumber += 1
      if (!Parse.isComment(line)) {
        val scLn = new java.util.Scanner(line).useDelimiter("[\t ,]+")

        try {
          val month = scLn.next()
          val day = scLn.nextInt()
          val year = scLn.nextInt()

          val date = LocalDate.apply(year, CoinMarketCapPrice.parseMonth(month), day)
          val open = scLn.nextDouble()
          val high = scLn.nextDouble()
          val low = scLn.nextDouble()
          val close = scLn.nextDouble()

          prices(date) = Config.config.priceCalculation match {
            case PriceCalculation.open      => open
            case PriceCalculation.close     => close
            case PriceCalculation.openClose => (open + close) / 2
            case PriceCalculation.high      => high
            case PriceCalculation.low       => low
            case PriceCalculation.lowHigh   => (high + low) / 2
          }
        } catch {
          case _ => Logger.warning(s"CoinMarketCapPrice. Could not read line $lineNumber '${line}' in file ${file.getName}")
        } finally {
          scLn.close()
        }
      }
    }
    sc.close()
    return prices
  }

  private lazy val prices = readPrices()

  def apply(date : LocalDateTime) : Price =
    prices.get(LocalDate.of(date)) match  {
      case None =>
        Logger.fatal(s"price for $market at $date not found")
      case Some(price) => price
    }

  def downloadPrices(): Unit = {
    val header = "Date\tOpen\tHigh\tLow\tClose"
    Logger.trace(s"Downloading prices for $market from coinmarketcap.com.")
    val lines = CoinMarketCapPrice.scrapPrices(coinMarketCapID)
    FileSystem.backup(fileFullPath)
    val ps = FileSystem.PrintStream(fileFullPath)
    ps.println(header)
    for(ln <- lines)
      ps.println(ln)
    ps.close()
  }
}
