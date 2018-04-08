package taxes.Exchanger

import taxes.Market.Market
import taxes.Util.Logger
import taxes.Util.Parse._
import taxes._

object Bittrex extends Exchanger {
  override val id: String = "Bittrex"

  override val sources = Seq(
    new UserFolderSource[Operation]("bittrex", ".csv") {
      def fileSource(fileName: String) = operationsReader(fileName)
    },
    new UserFolderSource[Operation]("bittrex/withdrawals", ".csv") {
      def fileSource(fileName: String) = withdrawalsReader(fileName)
    }
  )

  private def operationsReader(fileName: String) = new CSVSortedOperationReader(fileName) {
    override val hasHeader: Boolean = true

    override val charSet: String = "UTF-16LE"

    override def lineScanner(line: String) =
      SeparatedScanner(line, "[,]")

    override def readLine(line: String, scLn: Scanner): CSVReader.Result[Operation] = {
      val orderId = scLn.next("Order ID")
      val (market1, aux) = scLn.next("Pair").span(_ != '-')
      val market2 = aux.tail

      val isSell = scLn.next("Order Type") == "LIMIT_SELL"
      val quantity = scLn.nextDouble("Quantity")
      val limit = scLn.nextDouble("Limit")
      val comissionPaid = scLn.nextDouble("Comission Paid")

      val price = scLn.nextDouble("Price")

      val dateOpen = Date.fromString(scLn.next("Open Date") + " +0000", "MM/dd/yyyy hh:mm:ss a Z") // Bittrex time is 1 hour behind here
      val dateClose = Date.fromString(scLn.next("Close Date") + " +0000", "MM/dd/yyyy hh:mm:ss a Z")

      val desc = id + " " + orderId

      // fees are denominated in market1. market1 is normally BTC, USDT or ETH
      val exchange =
        if (isSell)
          Exchange(
            date = dateClose
            , id = orderId
            , fromAmount = quantity, fromMarket = Market.normalize(market2)
            , toAmount = price - comissionPaid, toMarket = Market.normalize(market1)
            , fee = comissionPaid, feeMarket = Market.normalize(market1)
            , exchanger = Bittrex
            , description = desc
          )
        else
          Exchange(
            date = dateClose
            , id = orderId
            , fromAmount = price + comissionPaid, fromMarket = Market.normalize(market1)
            , toAmount = quantity, toMarket = Market.normalize(market2)
            , fee = comissionPaid, feeMarket = Market.normalize(market1)
            , exchanger = Bittrex
            , description = desc
          )
      return CSVReader.Ok(exchange)
    }
  }

  private def withdrawalsReader(fileName: String) = new FileSource[Operation](fileName) {
    def withdrawalFee(market : Market, date : Date) : Double = {
      market match {
        case Market.bitcoin =>
          if(date.getYear<2017)
            0.0002
          else
            0.001
        case _ => 0
      }
    }

    def read() : Seq[Operation] = {
      val f = new java.io.File(fileName)
      val sc = new java.util.Scanner(f)

      def getNextLine(): Option[String] = {
        while (sc.hasNextLine) {
          val ln = Parse.trimSpaces(sc.nextLine())
          if (ln.nonEmpty)
            return Some(ln)
        }
        return None
      }

      var operations = List[Operation]()

      getNextLine() // skip header

      var ok = true
      while (sc.hasNextLine) {
        val opts = (0 to 2).map(_ => getNextLine())

        ok = opts.forall(_.isDefined)

        if (ok) {
          val scLn = opts.map { case Some(ln) => SeparatedScanner(ln, "[ \t]+") }
          try {
            val date = Date.fromString(scLn(0).next(), "MM/dd/yyyy")
            val currency = Market.normalize(scLn(0).next())
            val amount = scLn(0).nextDouble()
            val status = scLn(0).next()

            scLn(1).next()
            val address = scLn(1).next()

            scLn(2).next()
            val txid = scLn(2).next()

            val paidFee = withdrawalFee(currency, date)
            if (status == "Completed" && paidFee > 0) {
              val desc = id + " Withdrawal fee " + currency + " " + txid

              val fee = Fee(
                date = date
                , id = desc
                , amount = paidFee
                , market = currency
                , exchanger = Bittrex
                , description = desc
              )
              operations ::= fee
            } else
              Logger.warning("%s. Read withdrawal. This withdrawal was ignored: %s.".format(id, opts(0).get))
          } catch {
            case e => Logger.fatal("Something went wrong reading csv file %s. %s".format(fileName, e))
          } finally
            scLn.map(_.close())
        }
      }
      sc.close()
      return operations
    }
  }
}

