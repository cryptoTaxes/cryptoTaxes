package taxes.exchanger

import taxes._
import taxes.date._
import taxes.io.FileSystem
import taxes.util.Logger
import taxes.util.parse._


object Kraken extends Exchanger {
  override val id: String = "Kraken"

  private val configFileName = FileSystem.readConfigFile("krakenMarkets.txt")

  private val conversions: Map[Market, Market] =
    Parse.readAssociations(configFileName, "Reading Kraken markets")

  def parsePair(pair: String): (Market, Market) = {
    var found = false

    var baseMarket0 = ""
    var baseMarket = ""

    val it = conversions.keys.iterator
    while (!found && it.hasNext) {
      baseMarket0 = it.next()

      if (pair.startsWith(baseMarket0)) {
        found = true
        baseMarket = conversions(baseMarket0)
      }
    }

    if (!found)
      Logger.fatal(s"Could not parse Kraken pair $pair. Check file $configFileName.")

    val quoteMarket0 = pair.drop(baseMarket0.length)

    val quoteMarket = conversions.get(quoteMarket0) match {
      case None => Logger.fatal(s"Could not parse Kraken market $quoteMarket0. Check file $configFileName.")
      case Some(market) => market
    }

    return (
        Market.normalize(baseMarket)
      , Market.normalize(quoteMarket)
    )
  }


  private type TxID = String
  private case class Ledger(txType : String, market: Market, amount: Double, fee: Double)
  private val ledgersCache = scala.collection.mutable.Map[TxID, Ledger]()

  private type OrderID = String
  private val isMarginCache = scala.collection.mutable.Map[OrderID, Boolean]()

  override val sources = {
    val tradesFolder = "kraken/trades"
    Seq(
      // Reads deposits/withdrawals but also populates ledgersCache
      new UserInputFolderSource[Operation]("kraken/ledgers", ".csv") {
        def fileSource(fileName: String) = ledgerReader(fileName)
      },
      // This one is not really a reader but a preprocessor
      // for populating isMarginCache
      new UserInputFolderSource[Nothing](tradesFolder, ".csv") {
        def fileSource(fileName: String) = isMarginPreprocessReader(fileName)
      },
      new UserInputFolderSource[Operation](tradesFolder, ".csv") {
        def fileSource(fileName: String) = operationsReader(fileName)
      }
    )
  }

  private def operationsReader(fileName: String) = new CSVSortedOperationReader(fileName) {
    override val linesToSkip = 1

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
      val fee0 = scLn.nextDouble("Fee")
      val vol = scLn.nextDouble("Volume")
      val margin = scLn.nextDouble("Margin")
      val misc = scLn.next("Misc")
      val ledgers = scLn.next("Ledgers")

      val date = LocalDateTime.parseAsUTC(time, "yyyy-MM-dd HH:mm:ss.[SSSS][SSS][SS][S]") // kraken trades.csv uses UTC time zone
      val (baseMarket, quoteMarket) = parsePair(pair) // already normalized

      val id = txid + "/" + ordertxid
      val desc = "Order: " + id

      val isSell = sellBuy == "sell"
      val isBuy = sellBuy == "buy"
      val isMargin = isMarginCache(ordertxid)

      val ledgersTxids = Parse.sepBy(ledgers, ",")

      // we don't take into account rollover fees as these are not deductible
      val deductibleFees = Array("margin", "trade")
      val feeLedgers =
        ledgersTxids.flatMap(ledgersCache.get).filter(ledger => deductibleFees.contains(ledger.txType) && ledger.fee > 0)

      val (fee1, fee1Market, fee2, fee2Market) = feeLedgers match {
        case List() =>
          (0.0, baseMarket, 0.0, quoteMarket)   // no fees for this operation
        case List(feeLedger) =>                 // a single fee1
          (feeLedger.fee, feeLedger.market, 0.0, quoteMarket)
        case List(feeLedger1, feeLedger2) =>  // two fees for this operation
          // We assume feeLedger1.market != feeLedger2.market
          if(feeLedger1.market == feeLedger2.market)
            Logger.fatal(s"${Kraken.id}. Read file ${FileSystem.pathFromData(fileName)}: Reading this transaction is not currently supported as fee1 market is the same as fee2 market: $line.\n$feeLedger1\n$feeLedger2")
          (feeLedger1.fee, feeLedger1.market, feeLedger2.fee, feeLedger2.market)
        case ls =>
          Logger.fatal(s"${Kraken.id}. Read file ${FileSystem.pathFromData(fileName)}: Reading this transaction is not currently supported as it has more than two fees: $line.\n$ls")
      }

      if (isMargin) { // margin trade
        if(isSell) {
          val margin =
            Margin(
              date = date
              , id = id
              , fromAmount = vol, fromMarket = baseMarket
              , toAmount = cost /* + (if(fee1Market==quoteMarket) fee1 else -fee1 * price) */, toMarket = quoteMarket
              , fees = List(FeePair(fee1, fee1Market), FeePair(fee2, fee2Market))
              , orderType = Operation.OrderType.Sell
              , pair = (baseMarket, quoteMarket)
              , exchanger = Kraken
              , description = desc
            )
          return CSVReader.Ok(margin)
        } else if(isBuy) {
          val margin =
            Margin(
              date = date
              , id = id
              , fromAmount = cost /* + (if(fee1Market==quoteMarket) fee1 else -fee1 * price) */, fromMarket = quoteMarket
              , toAmount = vol, toMarket = baseMarket
              , fees = List(FeePair(fee1, fee1Market), FeePair(fee2, fee2Market))
              , orderType = Operation.OrderType.Buy
              , pair = (baseMarket, quoteMarket)
              , exchanger = Kraken
              , description = desc
            )
          return CSVReader.Ok(margin)
        } else
          return CSVReader.Warning(s"$id. Read file ${FileSystem.pathFromData(fileName)}: Reading this transaction is not currently supported: $line.")
    } else { // spot exchange
        if(isSell) {
            // If fee1Market==fromMarket, vol is just what we are exchanging
            // (hence it corresponds to fromAmount). As vol does not include
            // fee1, we don't have to subtract fee1 to define fromAmount.
            // cost doesn't include fee1 and stands directly for toAmount.

            // If fee1Market==toMarket, cost is what we are exchanging plus the fee1
            // hence cost - fee1 is what we are exchanging, so we have to compute the
            // subtraction to define toAmount.
            // vol doesn't include fee1 and stands directly for fromAmount.

            val toAmount =
              if(Config.config.deprecatedUp2017Version)
                cost - (if (fee1Market == quoteMarket) fee1 else 0)
              else
                cost - {
                  if(fee2Market == quoteMarket)
                    fee2 // we end up getting cost - fee2
                  else if(fee1Market == quoteMarket)
                    fee1 // we end up getting cost - fee1
                  else 0
                }

            val exchange =
              Exchange(
                date = date
                , id = id
                , fromAmount = vol
                , fromMarket = baseMarket
                , toAmount = toAmount
                , toMarket = quoteMarket
                , fees = List(FeePair(fee1, fee1Market), FeePair(fee2, fee2Market))
                , exchanger = Kraken
                , description = desc
              )
            return CSVReader.Ok(exchange)
        } else if(isBuy) {
            // If fee1Market==fromMarket, cost is just what we are exchanging
            // (hence it corresponds to fromAmount). As cost does not include
            // fee1, we don't have to subtract fee1 to define fromAmount.
            // Same goes for vol, which corresponds directly to toAmount.

            // If fee1Market== toMarket, vol is what we are exchanging plus the fee1
            // hence vol - fee1 is what we are exchanging, so we have to compute the
            // subtraction to define toAmount.
            // cost stands directly for fromAmount.

          val toAmount =
            if(Config.config.deprecatedUp2017Version)
              vol - (if (fee1Market == quoteMarket) fee1 else 0)
            else
              vol - {
                if(fee1Market == baseMarket)
                  fee1 // we end up getting cost - fee1
                else if(fee2Market == baseMarket)
                  fee2 // we end up getting cost - fee2
                else 0
              }

          val exchange =
              Exchange(
                date = date
                , id = id
                , fromAmount = cost
                , fromMarket = quoteMarket
                , toAmount = toAmount
                , toMarket = baseMarket
                , fees = List(FeePair(fee1, fee1Market), FeePair(fee2, fee2Market))
                , exchanger = Kraken
                , description = desc
              )
            return CSVReader.Ok(exchange)
        } else
          return CSVReader.Warning(s"$id. Read file ${FileSystem.pathFromData(fileName)}: Reading this transaction is not currently supported: $line.")
      }
    }
  }

  private def ledgerReader(fileName: String) = new CSVReader[Operation](fileName) {
    override val linesToSkip = 1

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
      val balance = scLn.next("Balance")

      val currency = Market.normalize(conversions.getOrElse(asset, asset))

      if (txType == "trade" || txType == "margin") {
        val ledger = Ledger(txType, currency, amount.abs, feeAmount)
        ledgersCache += (txid -> ledger)

        return CSVReader.Ignore
      } else if (txType == "withdrawal" && (currency == Market.bitcoin || currency == Market.euro)) {
        val date = LocalDateTime.parseAsUTC(time, "yyyy-MM-dd HH:mm:ss") // kraken ledgers.csv uses UTC time zone
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
        return CSVReader.Warning(s"$id. Read file ${FileSystem.pathFromData(fileName)}; Reading this transaction is not currently supported: $line.")
    }
  }

  private def isMarginPreprocessReader(fileName: String) = new CSVReader[Nothing](fileName) {
    override val linesToSkip = 1

    override def lineScanner(line: String): Scanner =
      QuotedScanner(line, '\"', ',')

    override def readLine(line: String, scLn: Scanner): CSVReader.Result[Nothing] = {
      val txid = scLn.next("Transaction ID")
      val ordertxid = scLn.next("Order ID")
      val pair = scLn.next("Pair")
      val time = scLn.next("Time")
      val sellBuy = scLn.next("Sell/Buy")
      val ordertype = scLn.next("Order Type")
      val price = scLn.nextDouble("Price")
      val cost = scLn.nextDouble("Cost")
      val fee0 = scLn.nextDouble("Fee")
      val vol = scLn.nextDouble("Volume")
      val margin = scLn.nextDouble("Margin")
      val misc = scLn.next("Misc")
      val ledgers = scLn.next("Ledgers")

      // only one txId for the OrderId need to have margin for it to be a margin order
      isMarginCache(ordertxid) = isMarginCache.getOrElse(ordertxid, false) || (margin != 0)
      return CSVReader.Ignore
    }
  }
}
