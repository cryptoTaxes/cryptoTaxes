package taxes.priceHistory

import java.util.InputMismatchException

import taxes._
import taxes.date._
import taxes.io.{FileSystem, Network}
import taxes.util.Logger


object EuroUSDParity {
  // see https://www.bde.es/webbde/es/estadis/infoest/tipos/tipos.html
  //     https://www.bde.es/webbde/es/estadis/infoest/series/tc_1_1.csv
  private def parseMonth(month: String) =
    month match {
      case "ENE" => 1
      case "FEB" => 2
      case "MAR" => 3
      case "ABR" => 4
      case "MAY" => 5
      case "JUN" => 6
      case "JUL" => 7
      case "AGO" => 8
      case "SEP" => 9
      case "OCT" => 10
      case "NOV" => 11
      case "DIC" => 12
    }

  private def readPrices(): scala.collection.mutable.Map[LocalDate, Price] = {
    val file = FileSystem.File(FileSystem.euroUSDFile)
    Logger.trace(s"Reading Euro prices from ${file.getAbsoluteFile}.")

    val prices = scala.collection.mutable.Map[LocalDate, Price]()
    var lineNumber = 0
    val sc = new java.util.Scanner(file, taxes.io.defaultCharset.name())
    val header1 = sc.nextLine()
    val header2 = sc.nextLine()
    val header3 = sc.nextLine()
    val header4 = sc.nextLine()
    var noPriceDates = List[LocalDate]()
    while(sc.hasNextLine) {
      val line = sc.nextLine()
      lineNumber += 1
      if(line.nonEmpty) {
        val scLn = new java.util.Scanner(line).useDelimiter("[,]+")
        try {
          val strDate = scLn.next().tail

          val day = strDate.take(2)
          val month = parseMonth(strDate.slice(3, 6))
          val year = strDate.slice(6, 10)
          val date = LocalDate.apply(year.toInt, month, day.toInt)

          try {
            val oneEuro = scLn.nextDouble()
            prices(date) = oneEuro
            noPriceDates match {
              case List() => ;
              case dates =>
                for(date <- dates.reverse)
                  prices(date) = oneEuro
                noPriceDates = List[LocalDate]()
            }
          } catch {
            case _: InputMismatchException =>
              noPriceDates ::= date // if no price was published we will use that of next day
          }
        } catch {
          case _ => Logger.warning(s"EuroUSDParity. Could not read line $lineNumber '$line' in file ${file.getName}")
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

  def oneEuro2USD(date: LocalDateTime): Price = {
    // uses price of next day if not found
    var attemptDate = LocalDate.of(date)
    while(attemptDate <= lastDay) {
      prices.get(attemptDate) match {
        case None =>
          attemptDate = attemptDate.plusDays(1)
        case Some(price) =>
          return price
      }
    }
    return Logger.fatal(s"oneEuro2USD. Price for Euro not found for day $attemptDate. Last download price is for $lastDay.")
  }

  def USD2Euro(amount: Double, date: LocalDateTime): Price =
    amount / oneEuro2USD(date)

  def euro2USD(amount: Double, date: LocalDateTime): Price =
    amount * oneEuro2USD(date)

  def downloadPrices(): Unit = {
    Logger.trace("Downloading prices for euros/usd from www.bde.es.")

    val url = "https://www.bde.es/webbde/es/estadis/infoest/series/tc_1_1.csv"
    Network.Http.withSource(url, charset = java.nio.charset.StandardCharsets.ISO_8859_1){ src =>
      FileSystem.withPrintStream(FileSystem.euroUSDFile) { ps =>
        for(line <- src.getLines())
          ps.println(line)
      }
    }
  }
}

