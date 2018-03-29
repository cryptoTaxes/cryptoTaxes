package taxes.Exchanger

import taxes.Market.Market
import taxes.Util.Logger
import taxes.Util.Parse._
import taxes._

object Kraken extends Exchanger with Initializable {
  override val id: String = "Kraken"

  private val configFileName = Paths.configFile("krakenMarkets.txt")

  private val conversions: Map[Market, Market] =
    Parse.readAssociations(configFileName, "Reading Kraken markets")

  def parsePair(pair: String): (Market, Market) = {
    var found = false

    var market1_0 = ""
    var market1 = ""

    val it = conversions.keys.iterator
    while (!found && it.hasNext) {
      market1_0 = it.next()

      if (pair.startsWith(market1_0)) {
        found = true
        market1 = conversions(market1_0)
      }
    }

    if (!found)
      Logger.fatal("Could not parse Kraken pair %s. Check file %s." format(pair, configFileName))

    val market2_0 = pair.drop(market1_0.length)

    val market2 = conversions.get(market2_0) match {
      case None => Logger.fatal("Could not parse Kraken market %s. Check file %s." format(market2_0, configFileName))
      case Some(market) => market
    }

    return (Market.normalize(market1)
      , Market.normalize(market2)
    )
  }

  override val sources = Seq(
    new UserFolderSource[Operation]("kraken/ledgers") {
      def fileSource(fileName: String) = ledgerReader(fileName)
    },
    new UserFolderSource[Operation]("kraken/trades") {
      def fileSource(fileName: String) = operationsReader(fileName)
    }
  )

  private type TxID = String
  private case class Ledger(market: Market, amount: Double, fee: Double)
  private val ledgersCache = scala.collection.mutable.Map[TxID, Ledger]()

  private def operationsReader(fileName: String) = new CSVSortedOperationReader(fileName) {
    override val hasHeader: Boolean = true

    override def lineScanner(line: String): Scanner =
      QuotedScanner(line, '\"', ',')

    override def readLine(line: String, scLn: Scanner): CSVReader.Result[Operation] = {
      val txid = scLn.next("Transaction ID")
      val ordertxid = scLn.next("Order ID")
      val pair = scLn.next("Pair")
      val time = scLn.next("Time")
      val sellBuy = scLn.next("Sell/Buy")
      val ordertype = scLn.next("Order Type")
      val price = scLn.nextDouble("Price")
      val cost = scLn.nextDouble("Cost")
      val fee = scLn.nextDouble("Fee")
      val vol = scLn.nextDouble("Volume")
      val margin = scLn.nextDouble("Margin")
      val misc = scLn.next("Misc")
      val ledgers = scLn.next("Ledgers")

      val date = Date.fromString(time + " +0000", "yyyy-MM-dd HH:mm:ss.S Z")
      val (market1, market2) = parsePair(pair)

      val id = txid + "/" + ordertxid
      val desc = Kraken + " " + id

      val (txidMarket2, txidMarket1) = Parse.split(ledgers, ",")

      val Ledger(_, amount1, fee1) = ledgersCache(txidMarket1)
      val Ledger(_, amount2, fee2) = ledgersCache(txidMarket2)


      if (sellBuy == "sell" /* && ordertype == "limit"*/ ) {

        val exchange =
          Exchange(
            date = date
            , id = id
            , fromAmount = amount1 + fee1, fromMarket = Market.normalize(market1)
            , toAmount = amount2 - fee2, toMarket = Market.normalize(market2)
            , fee = fee2
            , feeMarket = Market.normalize(market2)
            , exchanger = Kraken
            , description = desc
          )
        return CSVReader.Ok(exchange)
      } else if (sellBuy == "buy") {
        val feeInMarket1 = fee / price

        val exchange =
          Exchange(
            date = date
            , id = id
            , fromAmount = amount1 + fee1, fromMarket = Market.normalize(market2)
            , toAmount = amount2 - fee2, toMarket = Market.normalize(market1)
            , fee = fee1
            , feeMarket = Market.normalize(market2)
            , exchanger = Kraken
            , description = desc
          )
        return CSVReader.Ok(exchange)
      } else
        return CSVReader.Warning("%s. Read file: Reading this transaction is not currently supported: %s.".format(id, line))
    }
  }


  private def ledgerReader(fileName: String) = new CSVReader[Operation](fileName) {
    override val hasHeader: Boolean = true

    override def lineScanner(line: String): Scanner =
      QuotedScanner(line, '\"', ',')

    override def readLine(line: String, scLn: Scanner): CSVReader.Result[Operation] = {
      val txid = scLn.next("Transaction ID")
      val refid = scLn.next("Reference ID")
      val time = scLn.next("Time")
      val txType = scLn.next("Transaction Type")
      val aclass = scLn.next("Class")
      val asset = scLn.next("Asset")
      val amount = scLn.nextDouble("Amount")
      val feeAmount = scLn.nextDouble("Fee")
      val balance = scLn.nextDouble("Balance")

      val currency = Market.normalize(conversions.getOrElse(asset, asset))

      if (txType == "trade") {
        val ledger = Ledger(currency, amount.abs, feeAmount)
        ledgersCache += (txid -> ledger)

        return CSVReader.Ignore
      } else if (txType == "withdrawal" && (currency == Market.bitcoin || currency == Market.euro)) {
        val date = Date.fromString(time + " +0000", "yyyy-MM-dd HH:mm:ss Z")
        val id = txid + "/" + refid

        val fee = Fee(
          date = date
          , id = id
          , amount = feeAmount
          , market = currency
          , exchanger = Kraken
          , description = Kraken + " Withdrawal fee " + currency + " " + id
        )
        return CSVReader.Ok(fee)
      }  else
        return CSVReader.Warning("%s. Read file. Reading this transaction is not currently supported: %s.".format(id, line))
    }
  }
}
