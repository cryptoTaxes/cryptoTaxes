package taxes.Exchanger

import taxes.Market.Market
import taxes.Util.Logger
import taxes.Util.Parse.{CSVSortedOperationReader, Parse, QuotedScanner, Scanner}
import taxes._

object Kraken extends Exchanger with Initializable {
  override val id: String = "Kraken"

  private val configFileName = Paths.configFile("krakenMarkets.txt")

  private val conversions : Map[Market, Market] =
    Parse.readAssociations(configFileName, "Reading Kraken markets")

  def parsePair(pair : String) : (Market, Market) = {
    var found = false

    var market1_0 = ""
    var market1 = ""

    val it = conversions.keys.iterator
    while(!found && it.hasNext) {
      market1_0 = it.next()

      if(pair.startsWith(market1_0)) {
        found = true
        market1 = conversions(market1_0)
      }
    }

    if(!found)
      Logger.fatal("Could not parse Kraken pair %s. Check file %s." format (pair, configFileName))

    val market2_0 = pair.drop(market1_0.length)

    val market2 = conversions.get(market2_0) match {
      case None => Logger.fatal("Could not parse Kraken market %s. Check file %s." format (market2_0, configFileName))
      case Some(market) => market
    }

    return  ( Market.normalize(market1)
             , Market.normalize(market2)
             )
  }

  override val sources = Seq(
    new UserFolderSource[Operation]("kraken") {
      def fileSource(fileName : String) = operationsReader(fileName)
    }
  )

  private def operationsReader(fileName : String) = new CSVSortedOperationReader(fileName) {
    override val hasHeader: Boolean = true

    override def lineScanner(line: String): Scanner =
      QuotedScanner(line, '\"', ',')

    override def readLine(line: String, scLn: Scanner): Either[String, Operation] = {
      val txid = scLn.next()
      val ordertxid = scLn.next()
      val pair = scLn.next()
      val time = scLn.next()
      val sellBuy = scLn.next()
      val ordertype = scLn.next()
      val price = scLn.nextDouble()
      val cost = scLn.nextDouble()
      val fee = scLn.nextDouble()
      val vol = scLn.nextDouble()
      val margin = scLn.nextDouble()
      val misc = scLn.next()
      val ledgers = scLn.next()

      if(sellBuy == "sell" && ordertype == "limit") {
        val date = Date.fromString(time + " +0000", "yyyy-MM-dd HH:mm:ss.S Z")
        val (market1, market2) = parsePair(pair)

        val amount1 = vol
        val amount2 = cost

        val desc = id + " " + txid + "/" + ordertxid

        val exchange =
          Exchange(
            date = date
            , id = txid + "/" + ordertxid
            , fromAmount = amount1, fromMarket = Market.normalize(market1)
            , toAmount = amount2 - fee, toMarket = Market.normalize(market2)
            , fee = fee
            , feeMarket = Market.normalize(market2)
            , exchanger = Kraken
            , description = desc
          )
        return Right(exchange)
      } else
        return Left("%s. Read file: Reading this transaction is not currently supported: %s.".format(id, line))
    }
  }
}

