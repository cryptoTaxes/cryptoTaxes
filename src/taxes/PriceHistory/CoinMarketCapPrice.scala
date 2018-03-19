package taxes.PriceHistory

import java.io.PrintStream

import taxes.Market.Market
import taxes.Util.Logger
import taxes.Util.Parse.Parse
import taxes._

import scala.io.Source

object CoinMarketCapPrice extends Initializable {
  def apply(market : Market, fileFullPath : String, coinMarketCapID: String) : CoinMarketCapPrice =
    new CoinMarketCapPrice(market, fileFullPath, coinMarketCapID)

  private def parseMonth(month : String) : String =
    month match {
      case "Jan" => "01"
      case "Feb" => "02"
      case "Mar" => "03"
      case "Apr" => "04"
      case "May" => "05"
      case "Jun" => "06"
      case "Jul" => "07"
      case "Aug" => "08"
      case "Sep" => "09"
      case "Oct" => "10"
      case "Nov" => "11"
      case "Dec" => "12"
    }

  private def parsePair(t : (Market, String)) = t match {
    case (market1, market2) => (Market.normalize(market1), market2.toLowerCase())
  }

  private val allPairs = Parse.readAssociations (
    Paths.configFile("coinmarketcapMarkets.txt")
    , "Reading coinmarketcap markets"
    ).map(parsePair)


  private val markets2CoinMarketPrices = {
    for((market, id) <- allPairs)
      yield (market, CoinMarketCapPrice(market, Paths.coinMarketCap+"/"+market.toLowerCase+".txt", id))
  }

  // returns price in USD for a market at a given date
  def apply(market : Market, date : Date) : Price =
    markets2CoinMarketPrices.get(market) match  {
      case Some(prices) => prices(date.at00)
      case None => Logger.fatal("price for %s at %s not found".format(market, date))
    }

  private def scrapContents(line : String) : String = {
    val token1 = '>'
    val token2 = '<'
    if(!line.contains(token1)) {
      Logger.fatal("Error reading coinmarketcap prices: "+line+".")
    }
    val skipBeginning = line.dropWhile(_ != token1).tail

    if(!line.contains(token2)) {
      Logger.fatal("Error reading coinmarketcap prices: "+line+".")
    }
    val contents = skipBeginning.takeWhile(_ != token2)
    return contents
  }

  private def scrapPrices(coinMarketCapID: String) : List[String] = {
    val url = "https://coinmarketcap.com/currencies/"+coinMarketCapID+"/historical-data/?start=20130101&end=20500101"
    val src = Source.fromURL(url)
    /*
    val src = Source.fromFile(coinMarketCapID)
    Thread.sleep(5000)
    val ps = new PrintStream(coinMarketCapID)
    ps.println(src.mkString)
    ps.close()
    */
    val tokenBegin = "<table class=\"table\">"
    val tokenEnd = "</tbody>"

    val inLines = src.getLines().filter(_.nonEmpty).map(_.dropWhile(_.isSpaceChar)).dropWhile( _ != tokenBegin).drop(13)
    if(inLines.isEmpty)
      Logger.fatal("Error reading coinmarketcap prices. Couldn't find %s.".format(tokenBegin))

    var outLines = List[String]()

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

        outLines ::= List(date,open,high,low,close).mkString("\t")
      } else {
        goOn = false
        if(line1 != tokenEnd)
          Logger.fatal("Something went wrong scrapping prices for %s.".format(coinMarketCapID))
      }
    }
    src.close()
    return outLines.reverse
  }

  def downloadPrices(): Unit = {
    for(coinmarketcap <- markets2CoinMarketPrices.values)
      coinmarketcap.downloadPrices()
  }

}

class CoinMarketCapPrice(market : Market, fileFullPath : String, coinMarketCapID: String) extends PriceHistory {

  private def readPrices() : scala.collection.mutable.Map[Date, Price] = {
    Logger.trace("Reading CoinMarketPrice from " + fileFullPath + ".")
    val prices = scala.collection.mutable.Map[Date, Price]()
    val file = new java.io.File(fileFullPath)
    val sc = new java.util.Scanner(file)
    val header = sc.nextLine()
    var lineNumber = 0
    while (sc.hasNextLine) {
      val line = sc.nextLine()
      lineNumber += 1
      if (!Parse.isComment(line)) {
        val scLn = new java.util.Scanner(line).useDelimiter("[\t ]+")

        try {
          val month = scLn.next()
          val day = scLn.next()
          val year = scLn.next()

          val date = Date.fromString(month + day + year, "MMMdd,yyyy")
          val open = scLn.nextDouble()
          val high = scLn.nextDouble()
          val low = scLn.nextDouble()
          val close = scLn.nextDouble()
          val avg = (high + low) / 2
          prices(date) = avg
        } catch {
          case _ => Logger.warning("CoinMarketCapPrice. Could not read line %d \"%s\" in file %s" format(lineNumber, line, file.getName))
        } finally {
          scLn.close()
        }
      }
    }
    sc.close()
    return prices
  }

  private lazy val prices = readPrices()

  def apply(date : Date) : Price =
    prices.get(date.at00) match  {
      case None =>
        Logger.fatal("price for %s at %s not found".format(market, date))
      case Some(price) => price
    }

  def downloadPrices(): Unit = {
    val header = "Date\tOpen\tHigh\tLow\tClose"
    Logger.trace("Downloading prices for "+market+" from coinmarketcap.com.")
    val lines = CoinMarketCapPrice.scrapPrices(coinMarketCapID)
    Paths.backup(fileFullPath)
    val ps = new java.io.PrintStream(fileFullPath)
    ps.println(header)
    for(ln <- lines)
      ps.println(ln)
    ps.close()
  }

}
