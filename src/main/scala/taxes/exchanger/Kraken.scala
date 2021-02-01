package taxes.exchanger

import taxes._
import taxes.date._
import taxes.io.FileSystem
import taxes.util.Logger
import taxes.util.parse._


object Kraken extends Exchanger {
  override val id: String = "Kraken"

  val krakenCurrenciesFile = FileSystem.inConfigFolder("krakenCurrencies.txt")

  private val conversions: Map[Currency, Currency] =
    Parse.readKeysValue(krakenCurrenciesFile, "Reading Kraken currencies").map{
      case (krakenCurrency, currency) => (krakenCurrency.toUpperCase, Currency.normalize(currency))
    }

  def parsePair(pair: String): (Currency, Currency) = {
    var found = false

    var _baseCurrency = ""
    var baseCurrency = ""

    val it = conversions.keys.iterator
    while(!found && it.hasNext) {
      _baseCurrency = it.next()

      if(pair.startsWith(_baseCurrency)) {
        found = true
        baseCurrency = conversions(_baseCurrency)
      }
    }

    if(!found)
      Logger.fatal(s"Could not parse Kraken pair $pair. Check file $krakenCurrenciesFile.")

    val _quoteCurrency = pair.drop(_baseCurrency.length)

    val quoteCurrency = conversions.get(_quoteCurrency) match {
      case None => Logger.fatal(s"Could not parse Kraken currency ${_quoteCurrency}. Check file $krakenCurrenciesFile.")
      case Some(currency) => currency
    }

    return (
        Currency.normalize(baseCurrency)
      , Currency.normalize(quoteCurrency)
    )
  }

  private type TxID = String
  private case class Ledger(txType: String, currency: Currency, amount: Double, fee: Double)
  private val ledgersCache = scala.collection.mutable.Map[TxID, Ledger]()

  private type OrderID = String
  private val isMarginCache = scala.collection.mutable.Map[OrderID, Boolean]()

  private type RefID = String
  private case class OnChainInfo(address: String, tx: String)
  private val depositsWithdrawalsInfo = scala.collection.mutable.Map[RefID, OnChainInfo]()

  override val sources = {
    val tradesFolder = "kraken/trades"
    Seq(
      // reads extra on-chain information for deposits and withdrawals and populates depositsWithdrawalsInfo
      new FilteredUserInputFolderSource[Operation]("kraken/ledgers/depositsWithdrawals", ".txt") {
        def fileSource(fileName: String)= new FileSource[Nothing](fileName) {
          override def read(): Seq[Nothing] =
            readDepositsWithdrawals(fileName)
        }
      },
      // Reads deposits/withdrawals but also populates ledgersCache
      new FilteredUserInputFolderSource[Operation]("kraken/ledgers", ".csv") {
        def fileSource(fileName: String) = ledgerReader(fileName)
      },
      new FilteredUserInputFolderSource[Operation](tradesFolder, ".csv") {
        def fileSource(fileName: String) = operationsReader(fileName)
      }
    )
  }

  private def operationsReader(fileName: String) = new CSVSortedOperationReader(fileName) {
    // A preprocessor for populating isMarginCache
    override def preprocess() = Some { () =>
      Logger.trace(s"Determining margin trades from $fileName.")
      isMarginPreprocessReader(fileName).read()
    }

    override val linesToSkip = 1

    private lazy val provider = AssociativeQuotedScannerProvider(skippedLines(0), '\"', ',')
    override def lineScanner(line: String): Scanner =
      provider.scannerFor(line)

    override def readLine(line: String, scLn: Scanner): CSVReader.Result[Operation] = {
      val txid = scLn.next("txid")
      val ordertxid = scLn.next("ordertxid")
      val pair = scLn.next("pair")
      val time = scLn.next("time")
      val ordertype = scLn.next("type")
      val cost = scLn.nextDouble("cost")
      val vol = scLn.nextDouble("vol")
      val margin = scLn.nextDouble("margin")
      val ledgers = scLn.next("ledgers")

      val date = LocalDateTime.parseAsUTC(time, "yyyy-MM-dd HH:mm:ss.[SSSS][SSS][SS][S]") // kraken trades.csv uses UTC time zone
      val (baseCurrency, quoteCurrency) = parsePair(pair) // already normalized

      val id = txid + "/" + ordertxid
      val desc = RichText(s"Order: $id")

      val isSell = ordertype == "sell"
      val isBuy = ordertype == "buy"
      val isMargin = isMarginCache(ordertxid)

      val ledgersTxids = Parse.sepBy(ledgers, ",")

      // we don't take into account rollover fees as these are not deductible
      val deductibleFees = Array("margin", "trade")
      val feeLedgers =
        ledgersTxids.flatMap(ledgersCache.get).filter(ledger => deductibleFees.contains(ledger.txType) && ledger.fee > 0)

      val (fee1, fee1Currency, fee2, fee2Currency) = feeLedgers match {
        case List() =>
          (0.0, baseCurrency, 0.0, quoteCurrency)   // no fees for this operation
        case List(feeLedger) =>                     // a single fee1
          (feeLedger.fee, feeLedger.currency, 0.0, quoteCurrency)
        case List(feeLedger1, feeLedger2) =>  // two fees for this operation
          // We assume feeLedger1.currency != feeLedger2.currency
          if(feeLedger1.currency == feeLedger2.currency)
            Logger.fatal(s"${Kraken.id}. Read file ${FileSystem.pathFromData(fileName)}: Reading this transaction is not currently supported as fee1 currency is the same as fee2 currency: $line.\n$feeLedger1\n$feeLedger2")
          (feeLedger1.fee, feeLedger1.currency, feeLedger2.fee, feeLedger2.currency)
        case ls =>
          Logger.fatal(s"${Kraken.id}. Read file ${FileSystem.pathFromData(fileName)}: Reading this transaction is not currently supported as it has more than two fees: $line.\n$ls")
      }

      if(isMargin) { // margin trade
        val leverage = if (margin==0) 1 else (cost / margin).round.toInt
        if(isSell) {
          val margin =
            Margin(
              date = date
              , id = id
              , fromAmount = vol
              , fromCurrency = baseCurrency
              , toAmount = cost /* + (if(fee1Currency==quoteCurrency) fee1 else -fee1 * price) */
              , toCurrency = quoteCurrency
              , fees = List(FeePair(fee1, fee1Currency), FeePair(fee2, fee2Currency))
              , orderType = Operation.OrderType.Sell
              , pair = (baseCurrency, quoteCurrency)
              , exchanger = Kraken
              , description = desc
            )
          return CSVReader.Ok(margin)
        } else if(isBuy) {
          val margin =
            Margin(
              date = date
              , id = id
              , fromAmount = cost /* + (if(fee1Currency==quoteCurrency) fee1 else -fee1 * price) */
              , fromCurrency = quoteCurrency
              , toAmount = vol
              , toCurrency = baseCurrency
              , fees = List(FeePair(fee1, fee1Currency), FeePair(fee2, fee2Currency))
              , orderType = Operation.OrderType.Buy
              , pair = (baseCurrency, quoteCurrency)
              , exchanger = Kraken
              , description = desc
            )
          return CSVReader.Ok(margin)
        } else
          return CSVReader.Warning(s"$id. Read file ${FileSystem.pathFromData(fileName)}: Reading this transaction is not currently supported: $line.")
    } else { // spot exchange
        if(isSell) {
            // If fee1Currency==fromCurrency, vol is just what we are exchanging
            // (hence it corresponds to fromAmount). As vol does not include
            // fee1, we don't have to subtract fee1 to define fromAmount.
            // cost doesn't include fee1 and stands directly for toAmount.

            // If fee1Currency==toCurrency, cost is what we are exchanging plus the fee1
            // hence cost - fee1 is what we are exchanging, so we have to compute the
            // subtraction to define toAmount.
            // vol doesn't include fee1 and stands directly for fromAmount.

            val toAmount =
              if(Config.config.deprecatedUp2017Version)
                cost - (if(fee1Currency == quoteCurrency) fee1 else 0)
              else
                cost - {
                  if(fee1Currency == quoteCurrency && fee2Currency == quoteCurrency)
                    fee1 + fee2
                  else if(fee2Currency == quoteCurrency)
                    fee2 // we end up getting cost - fee2
                  else if(fee1Currency == quoteCurrency)
                    fee1 // we end up getting cost - fee1
                  else 0
                }

            val exchange =
              Exchange(
                date = date
                , id = id
                , fromAmount = vol
                , fromCurrency = baseCurrency
                , toAmount = toAmount
                , toCurrency = quoteCurrency
                , fees = List(FeePair(fee1, fee1Currency), FeePair(fee2, fee2Currency))
                , exchanger = Kraken
                , description = desc
              )
            return CSVReader.Ok(exchange)
        } else if(isBuy) {
            // If fee1Currency==fromCurrency, cost is just what we are exchanging
            // (hence it corresponds to fromAmount). As cost does not include
            // fee1, we don't have to subtract fee1 to define fromAmount.
            // Same goes for vol, which corresponds directly to toAmount.

            // If fee1Currency== toCurrency, vol is what we are exchanging plus the fee1
            // hence vol - fee1 is what we are exchanging, so we have to compute the
            // subtraction to define toAmount.
            // cost stands directly for fromAmount.

          val toAmount =
            if(Config.config.deprecatedUp2017Version)
              vol - (if(fee1Currency == quoteCurrency) fee1 else 0)
            else
              vol - {
                if(fee1Currency == baseCurrency && fee2Currency == baseCurrency)
                  fee1 + fee2
                else if(fee1Currency == baseCurrency)
                  fee1 // we end up getting cost - fee1
                else if(fee2Currency == baseCurrency)
                  fee2 // we end up getting cost - fee2
                else 0
              }

          val exchange =
              Exchange(
                date = date
                , id = id
                , fromAmount = cost
                , fromCurrency = quoteCurrency
                , toAmount = toAmount
                , toCurrency = baseCurrency
                , fees = List(FeePair(fee1, fee1Currency), FeePair(fee2, fee2Currency))
                , exchanger = Kraken
                , description = desc
              )
            return CSVReader.Ok(exchange)
        } else
          return CSVReader.Warning(s"$id. Read file ${FileSystem.pathFromData(fileName)}: Reading this transaction is not currently supported: $line.")
      }
    }
  }

  def readDepositsWithdrawals(fileName: String): Seq[Nothing] = {
    FileSystem.withSource(fileName) { src =>
      val lines = src.getLines().filterNot(taxes.util.parse.Parse.isComment)

      while(lines.hasNext) {
        val Array(refId, address, tx) = lines.take(3).toArray
        depositsWithdrawalsInfo(refId) = OnChainInfo(address, tx)
      }
      return Seq()
    }
  }

  private def ledgerReader(fileName: String) = new CSVReader[Operation](fileName) {
    override val linesToSkip = 1
    lazy val header = skippedLines(0)

    private lazy val provider = AssociativeQuotedScannerProvider(skippedLines(0), '\"', ',')
    override def lineScanner(line: String): Scanner =
      provider.scannerFor(line)

    override def readLine(line: String, scLn: Scanner): CSVReader.Result[Operation] = {
      val txid = scLn.next("txid")
      val refid = scLn.next("refid")
      val time = scLn.next("time")
      val txType = scLn.next("type")
      val asset = scLn.next("asset")
      val amount = scLn.nextDouble("amount")
      val feeAmount = scLn.nextDouble("fee")

      val currency = Currency.normalize(conversions.getOrElse(asset, asset))

      if(txType == "trade" || txType == "margin") {
        val ledger = Ledger(txType, currency, amount.abs, feeAmount)
        ledgersCache += (txid -> ledger)

        return CSVReader.Ignore
      } else if(txType == "withdrawal"
                  && txid.nonEmpty) { // kraken annotates withdrawals twice. Only valid one has a non-empty txid

        val date = LocalDateTime.parseAsUTC(time, "yyyy-MM-dd HH:mm:ss") // kraken ledgers.csv uses UTC time zone
        val id = txid + "/" + refid

        val desc = depositsWithdrawalsInfo.get(refid) match {
          case None =>
            RichText(s"Withdrawal $currency $id")
          case Some(Kraken.OnChainInfo(address, tx)) =>
            RichText(s"Withdrawal ${RichText.util.transaction(currency, tx, address)}")
        }

        val withdrawal = Withdrawal(
          date = date
          , id = id
          , amount = amount.abs
          , currency = currency
          , exchanger = Kraken
          , description = desc // RichText(s"Withdrawal $currency $id")
        )

        var results = List[Operation](withdrawal)
        if((currency == Currency.bitcoin || currency == Currency.euro || Config.config.fundingFees) && !Config.config.deprecatedUp2017Version) {
          val fee = Fee(
            date = date
            , id = id
            , amount = feeAmount
            , currency = currency
            , exchanger = Kraken
            , description = RichText(s"Kraken withdrawal fee $currency $id")
          )
          results ++= List(fee)
        } else {
          val nonTaxableFee = NonTaxableFee(
            date = date
            , id = id
            , amount = feeAmount
            , currency = currency
            , exchanger = Kraken
            , description = RichText(s"Kraken withdrawal non taxable fee $currency $id")
          )
          results ++= List(nonTaxableFee)
        }
        return CSVReader.Ok(results)
      } else if(txType == "deposit" && txid.nonEmpty) {
        // kraken annotates deposits twice. The only valid one has a non-empty txid
        val date = LocalDateTime.parseAsUTC(time, "yyyy-MM-dd HH:mm:ss") // kraken ledgers.csv uses UTC time zone
        val id = txid + "/" + refid

        val desc = depositsWithdrawalsInfo.get(refid) match {
          case None =>
            RichText(s"Deposit $currency $id")
          case Some(Kraken.OnChainInfo(address, tx)) =>
            RichText(s"Deposit ${RichText.util.transaction(currency, tx, address)}")
        }

        val deposit = Deposit(
          date = date
          , id = id
          , amount = amount
          , currency = currency
          , exchanger = Kraken
          , description = desc // RichText(s"Deposit $currency $id")
        )
        var results = List[Operation](deposit)
        if((currency == Currency.bitcoin || currency == Currency.euro || Config.config.fundingFees) && !Config.config.deprecatedUp2017Version) {
          if(feeAmount > 0) {
            val fee = Fee(
              date = date
              , id = id
              , amount = feeAmount
              , currency = currency
              , exchanger = Kraken
              , description = RichText(s"Kraken Deposit fee $currency $id")
            )
            results ++= List(fee)
          } else if(feeAmount < 0) {
            val deposit = Deposit(
              date = date
              , id = id
              , amount = -feeAmount
              , currency = currency
              , exchanger = Kraken
              , description = RichText(s"Deposit $currency $id")
            )
            results ++= List(deposit)
          }
        }
        return CSVReader.Ok(results)
      } else if(txType == "rollover") {
        val date = LocalDateTime.parseAsUTC(time, "yyyy-MM-dd HH:mm:ss") // kraken ledgers.csv uses UTC time zone
        val id = txid + "/" + refid

        val nonTaxableFee = NonTaxableFee(
          date = date
          , id = id
          , amount = feeAmount
          , currency = currency
          , exchanger = Kraken
          , description = RichText(s"Non Taxable Fee $currency $id")
        )
        return CSVReader.Ok(nonTaxableFee)
      } else if(txType == "transfer") {
        val date = LocalDateTime.parseAsUTC(time, "yyyy-MM-dd HH:mm:ss") // kraken ledgers.csv uses UTC time zone
        val id = txid + "/" + refid

        val deposit = Deposit(
          date = date
          , id = id
          , amount = amount
          , currency = currency
          , exchanger = Kraken
          , description = RichText(s"Transfer $currency $id")
        )
        return CSVReader.Ok(deposit)
      } else
        return CSVReader.Warning(s"$id. Read file ${FileSystem.pathFromData(fileName)}; Reading this transaction is not currently supported: $line.")
    }
  }

  private def isMarginPreprocessReader(fileName: String) = new CSVReader[Nothing](fileName) {
    override val linesToSkip = 1

    private lazy val provider = AssociativeQuotedScannerProvider(skippedLines(0), '\"', ',')
    override def lineScanner(line: String): Scanner =
      provider.scannerFor(line)

    override def readLine(line: String, scLn: Scanner): CSVReader.Result[Nothing] = {
      val ordertxid = scLn.next("ordertxid")
      val margin = scLn.nextDouble("margin")

      // only one txId for the OrderId needs to include margin for it to be a margin order
      isMarginCache(ordertxid) = isMarginCache.getOrElse(ordertxid, false) || (margin != 0)
      return CSVReader.Ignore
    }
  }
}
