package taxes.PriceHistory

import java.util.InputMismatchException

import taxes.Util.Logger
import taxes._

import scala.io.Source

object EuroUSDParity extends Initializable {
  // see https://www.bde.es/webbde/es/estadis/infoest/tipos/tipos.html
  //     https://www.bde.es/webbde/es/estadis/infoest/series/tc_1_1.csv
  private def readMonth(month : String) =
    month match {
      case "ENE" => "01"
      case "FEB" => "02"
      case "MAR" => "03"
      case "ABR" => "04"
      case "MAY" => "05"
      case "JUN" => "06"
      case "JUL" => "07"
      case "AGO" => "08"
      case "SEP" => "09"
      case "OCT" => "10"
      case "NOV" => "11"
      case "DIC" => "12"
    }

  private def readPrices() : scala.collection.mutable.Map[Date, Price] = {
    val file = new java.io.File(Paths.euroUSDFile)
    Logger.trace("Reading Euro prices from " + file.getAbsoluteFile + ".")

    val prices = scala.collection.mutable.Map[Date, Price]()
    var lineNumber = 0
    val sc = new java.util.Scanner(file)
    val header1 = sc.nextLine()
    val header2 = sc.nextLine()
    val header3 = sc.nextLine()
    val header4 = sc.nextLine()
    var noPriceDates = List[Date]()
    while (sc.hasNextLine) {
      val line = sc.nextLine()
      lineNumber += 1
      if (line.nonEmpty) {
        val scLn = new java.util.Scanner(line).useDelimiter("[,]+")
        try {
          val strDate = scLn.next().tail

          val day = strDate.take(2)
          val month = readMonth(strDate.slice(3, 6))
          val year = strDate.slice(6, 10)

          val date = Date.fromString(month + day + year, "MMddyyyy")
          try {
            val oneEuro = scLn.nextDouble()
            prices(date) = oneEuro
            noPriceDates match {
              case List() => ;
              case dates =>
                for(date <- dates.reverse)
                  prices(date) = oneEuro
                noPriceDates = List[Date]()
            }
          } catch {
            case (_: InputMismatchException) =>
              noPriceDates ::= date // if no price was published we will use that of next day
          }
        } catch {
          case _ => Logger.warning("EuroUSDParity. Could not read line %d \"%s\" in file %s" format(lineNumber, line, file.getName))
        } finally {
          scLn.close()
        }
      }
    }
    sc.close()
    return prices
  }

  private lazy val prices = readPrices()

  private lazy val lastDay = prices.keys.max

  def oneEuro2USD(date : Date) : Price = {
    // uses price of next day if not found
    var attemptDate = date.at00
    while(attemptDate <= lastDay) {
      prices.get(attemptDate) match {
        case None =>
          attemptDate = attemptDate.nextDay
        case Some(price) =>
          return price
      }
    }
    return Logger.fatal("oneEuro2USD. Price for Euro not found for day %s. Last download price is for %s." format(attemptDate, lastDay))
  }

  def USD2Euro(amount : Double, date : Date) : Price =
    amount / oneEuro2USD(date)

  def euro2USD(amount : Double, date : Date) : Price =
    amount * oneEuro2USD(date)

  def downloadPrices(): Unit = {
    Logger.trace("Downloading prices for euros/usd from www.bde.es.")

    val url = "https://www.bde.es/webbde/es/estadis/infoest/series/tc_1_1.csv"
    val source = Source.fromURL(url)(scala.io.Codec.ISO8859)
    val fileName = Paths.euroUSDFile
    Paths.backup(fileName)
    val ps = new java.io.PrintStream(fileName)
    ps.print(source.mkString)
    ps.close()
  }

}

