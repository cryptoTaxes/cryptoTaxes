package taxes.exchanger

import taxes._
import taxes.date._
import taxes.io.FileSystem
import taxes.util.Logger
import taxes.util.parse._

import scala.collection.mutable.ListBuffer


object Bittrex extends Exchanger {
  override val id: String = "Bittrex"

  override val sources = Seq(
    new UserInputFolderSource[Operation]("bittrex", ".csv") {
      def fileSource(fileName: String) = operationsReader(fileName)
    },
    new UserInputFolderSource[Operation]("bittrex/deposits", ".csv") {
      def fileSource(fileName: String) = depositsReader(fileName)
    },
    new UserInputFolderSource[Operation]("bittrex/withdrawals", ".csv") {
      def fileSource(fileName: String) = withdrawalsReader(fileName)
    },
    new UserInputFolderSource[Operation]("bittrex/deposits/txt", ".txt") {
      def fileSource(fileName: String)= new FileSource[Operation](fileName) {
        override def read(): Seq[Operation] =
          readTxtDeposits(fileName)
      }
    },
    new UserInputFolderSource[Operation]("bittrex/withdrawals/txt", ".txt") {
      def fileSource(fileName: String)= new FileSource[Operation](fileName) {
        override def read(): Seq[Operation] =
          readTxtWithdrawals(fileName)
      }
    }
  )

  // This is for the csv format used by Bittrex from 2014 till 2017
  private def operationsReader(fileName: String) = new CSVSortedOperationReader(fileName) {
    override val linesToSkip = 1

    private val (isUTF16LE, hasBOM) = FileSystem.looksLikeUTF16LE(fileName)
    override val charset: String =
      if(isUTF16LE)
        if(hasBOM) "x-UTF-16LE-BOM" else "UTF-16LE"
      else
        "UTF-8"

    override def lineScanner(line: String) =
      SeparatedScanner(line, "[,]")

    private val header2014_2017 = "OrderUuid,Exchange,Type,Quantity,Limit,CommissionPaid,Price,Opened,Closed"
    private val header2018 = "Uuid,Exchange,TimeStamp,OrderType,Limit,Quantity,QuantityRemaining,Commission,Price,PricePerUnit,IsConditional,Condition,ConditionTarget,ImmediateOrCancel,Closed"

    // these must be lazy as we don't want to check them until lines have been already skipped
    private lazy val is2014_2017Format = skippedLines(0) == header2014_2017
    private lazy val is2018Format = skippedLines(0) == header2018

    override def readLine(line: String, scLn: Scanner): CSVReader.Result[Operation] =
      if(is2014_2017Format)
        readLine2014_2017(line, scLn)
      else if(is2018Format)
        readLine2018(line, scLn)
      else
        Logger.fatal(s"Error reading Bittrex order history.\nFile: $fileName.\nUnknown header: ${skippedLines(0)}")

    // This is for the csv format used by Bittrex from 2014 till 2017
    def readLine2014_2017(line: String, scLn: Scanner): CSVReader.Result[Operation] = {
      val orderId = scLn.next("OrderUuid")
      val (currency1, currency2) = Parse.split(scLn.next("Exchange"), "-")

      val quoteCurrency = Currency.normalize(currency1)
      val baseCurrency = Currency.normalize(currency2)

      val isSell = scLn.next("Type") == "LIMIT_SELL"
      val quantity = scLn.nextDouble("Quantity")
      val limit = scLn.nextDouble("Limit")
      val comissionPaid = scLn.nextDouble("Commission Paid")

      val price = scLn.nextDouble("Price")

      val fmt = "[M][MM]/[d][dd]/yyyy [h][hh]:mm:ss a"
      val dateOpen = LocalDateTime.parseAsUTC(scLn.next("Opened"), fmt)   // Bittrex csv trade history uses UTC time zone
      val dateClose = LocalDateTime.parseAsUTC(scLn.next("Closed"), fmt) // but notice that the GUI uses your local time

      val desc = RichText(s"Order: $orderId")

      // quoteCurrency is one of BTC, USDT or ETH.
      // quantity stands for the amount of baseCurrency coins you're either selling or buying.
      // fees are ALWAYS denominated in quoteCurrency.
      // price stands for:
      //  * In a sell: the amount of quoteCurrency coins you get from this operation,
      //               but then you'll have to pay comissionPaid from these.
      //  * In a buy: the amount of quoteCurrency coins you're paying in this exchange,
      //               but you'll have to additionally pay comissionPaid
      // In this way:
      // * In a sell: you release quantity coins. You get price coins, but then you'll pay
      //              the comission from these, so really you end up getting (price - comissionPaid) coins
      // * In a buy:  you release (price + comissionPaid) coins and you get quantity coins.

      val exchange =
        if(isSell)
          Exchange(
            date = dateClose
            , id = orderId
            , fromAmount = quantity, fromCurrency = baseCurrency
            , toAmount = price - comissionPaid, toCurrency = quoteCurrency
            , fees = List(FeePair(comissionPaid, quoteCurrency))
            , exchanger = Bittrex
            , description = desc
          )
        else
          Exchange(
            date = dateClose
            , id = orderId
            , fromAmount = price, fromCurrency = quoteCurrency
            , toAmount = quantity, toCurrency = baseCurrency
            , fees = List(FeePair(comissionPaid, quoteCurrency))
            , exchanger = Bittrex
            , description = desc
          )
      return CSVReader.Ok(exchange)
    }

    // This is for the csv format used by Bittrex from 2018 onwards
    def readLine2018(line: String, scLn: Scanner): CSVReader.Result[Operation] = {
      val orderId = scLn.next("Uuid")
      val (m1, m2) = Parse.split(scLn.next("Exchange"), "-")

      val quoteCurrency = Currency.normalize(m1)
      val baseCurrency = Currency.normalize(m2)

      val fmt = "[M][MM]/[d][dd]/yyyy [h][hh]:mm:ss a"
      val dateOpen = LocalDateTime.parseAsUTC(scLn.next("TimeStamp"), fmt)   // Bittrex csv trade history uses UTC time zone
      // but notice that the GUI uses your local time
      val isSell = scLn.next("Order Type") == "LIMIT_SELL"
      val limit = scLn.nextDouble("Limit")
      val quantity = scLn.nextDouble("Quantity")
      val quantityRemaining = scLn.nextDouble("QuantityRemaining")
      val comission = scLn.nextDouble("Commission")
      val price = scLn.nextDouble("Price")
      val pricePerUnit = scLn.nextDouble("PricePerUnit")
      val isConditional = scLn.next("IsConditional") == "True"

      val condition = scLn.next("Condition")
      val conditionTarget = scLn.next("ConditionTarget")
      val immediateOrCancel = scLn.next("ImmediateOrCancel") == "True"

      val dateClose = LocalDateTime.parseAsUTC(scLn.next("Close Date"), fmt)

      val desc = RichText(s"Order: $orderId")

      // quoteCurrency is one of BTC, USDT or ETH.
      // quantity - quantityRemaining stands for the amount of baseCurrency
      // coins you're either selling or buying.
      // fees are ALWAYS denominated in quoteCurrency.
      // price stands for:
      //  * In a sell: the amount of quoteCurrency coins you get from this operation,
      //               but then you'll have to pay comission from these.
      //  * In a buy: the amount of quoteCurrency coins you're paying in this exchange,
      //               but you'll have to additionally pay comission
      // In this way:
      // * In a sell: you release quantity - quantityRemaining coins. You get price coins, but then you'll pay
      //              the comission from these, so really you end up getting (price - comission) coins
      // * In a buy:  you release (price + comission) coins and you get quantity - quantityRemaining coins.

      val exchange =
        if(isSell)
          Exchange(
            date = dateClose // if(Config.config.deprecatedUp2017Version) dateClose else dateOpen
            , id = orderId
            , fromAmount = quantity - quantityRemaining, fromCurrency = baseCurrency
            , toAmount = price - comission, toCurrency = quoteCurrency
            , fees = List(FeePair(comission, quoteCurrency))
            , exchanger = Bittrex
            , description = desc
          )
        else
          Exchange(
            date = dateClose // if(Config.config.deprecatedUp2017Version) dateClose else dateOpen
            , id = orderId
            , fromAmount = price, fromCurrency = quoteCurrency
            , toAmount = quantity - quantityRemaining, toCurrency = baseCurrency
            , fees = List(FeePair(comission, quoteCurrency))
            , exchanger = Bittrex
            , description = desc
          )
      return CSVReader.Ok(exchange)
    }

    // toDo some bittrex operations have same close date so their order will depend on order in csv file which can change for different downloads
  }

  private def depositsReader(fileName: String) = new CSVSortedOperationReader(fileName) {
    override val linesToSkip = 1

    override def lineScanner(line: String): Scanner =
      SeparatedScanner(line, "[,]")

    override def readLine(line: String, scLn: Scanner): CSVReader.Result[Operation] = {
      val id = scLn.next("Id")
      val currency = Currency.normalize(scLn.next("Currency"))
      val amount = scLn.nextDouble("Amount")
      val confirmations = scLn.nextInt("Confirmations")

      val fmt = "yyyy-[M][MM]-[d][dd] [H][HH]:mm:ssX"
      val lastUpdatedDate = LocalDateTime.parseAsUTC(scLn.next("LastUpdatedDate"), fmt)

      val txid = scLn.next("TxId")
      val address = scLn.next("CryptoAddress")

      val desc = RichText(s"Deposit ${RichText.util.transaction(currency, txid, address)}")

      val deposit = Deposit(
        date = lastUpdatedDate
        , id = txid
        , amount = amount
        , currency = currency
        , exchanger = Bittrex
        , description = desc
      )
      return CSVReader.Ok(deposit)
    }
  }

  private def withdrawalsReader(fileName: String) = new CSVSortedOperationReader(fileName) {
    override val linesToSkip = 1

    override def lineScanner(line: String): Scanner =
      SeparatedScanner(line, "[,]")

    override def readLine(line: String, scLn: Scanner): CSVReader.Result[Operation] = {
      val paymentUuid = scLn.next("PaymentUuid")
      val currency = Currency.normalize(scLn.next("Currency"))
      val amount = scLn.nextDouble("Amount")
      val address = scLn.next("Address")

      val fmt = "yyyy-[M][MM]-[d][dd] [H][HH]:mm:ssX"
      val dateOpen = LocalDateTime.parseAsUTC(scLn.next("OpenedDate"), fmt)

      val authorized = scLn.next("Authorized") == "true"
      val pending = scLn.next("Pending") == "true"
      val txFee = scLn.nextDouble("TxFee")
      val cancelled = scLn.next("Cancelled") == "true"


      if(authorized && !cancelled) {
        val txid = scLn.next("TxId")
        val desc = RichText(s"Withdrawal ${RichText.util.transaction(currency, txid, address)}")

        val withdrawal = Withdrawal(
          date = dateOpen
          , id = txid
          , amount = amount
          , currency = currency
          , exchanger = Bittrex
          , description = desc
        )

        val fee =
          if(Config.config.fundingFees)
            Fee(
              date = dateOpen
              , id = txid
              , amount = txFee
              , currency = currency
              , exchanger = Bittrex
              , description = RichText(s"Bittrex withdrawal fee $currency $txid")
            )
          else
            NonTaxableFee(
              date = dateOpen
              , id = txid
              , amount = txFee
              , currency = currency
              , exchanger = Bittrex
              , description = RichText(s"Bittrex withdrawal non taxable fee $currency $txid")
            )
        return CSVReader.Ok(withdrawal)
      } else
        CSVReader.Warning(s"$id. Read withdrawal ${FileSystem.pathFromData(fileName)}: This withdrawal was not completed: $line.")
    }
  }

  object TxtUtils {
    def parseDate(str: String): LocalDateTime =
      LocalDateTime.parseAsMyZoneId(str, "yyyy/MM/dd HH:mm:ss")
    def parseAmount(str: String): Double =
      Parse.asDouble(str.filter(_ != ','))
    def parseTxid(str: String): String = Parse.removeSuffix(str, "copy") match {
      case None => str
      case Some(txid) => txid
    }

  }
  private def readTxtDeposits(fileName: String): Seq[Operation] = {
    FileSystem.withSource(fileName) { src =>
      val lines = src.getLines().filterNot(taxes.util.parse.Parse.isComment)
      val operations = ListBuffer[Operation]()
      while(lines.hasNext) {
        val Array(_,txidStr,_,address,_,dateStr,_,currencyStr,_,amountStr,_,_) = lines.take(12).toArray

        val txid = TxtUtils.parseTxid(txidStr)
        val date = TxtUtils.parseDate(dateStr)
        val currency = Currency.normalize(currencyStr)
        val amount = TxtUtils.parseAmount(amountStr)

        val desc = RichText(s"Deposit ${RichText.util.transaction(currency, txid, address)}")

        val deposit = Deposit(
          date = date
          , id = txid
          , amount = amount
          , currency = currency
          , exchanger = Bittrex
          , description = desc
        )

        operations += deposit
      }
      return operations.toList
    }
  }

  private def readTxtWithdrawals(fileName: String): Seq[Operation] = {
    FileSystem.withSource(fileName) { src =>
      val lines = src.getLines().filterNot(taxes.util.parse.Parse.isComment)
      val operations = ListBuffer[Operation]()
      while(lines.hasNext) {
        val Array(_,txidStr,_,address,_,dateStr,_,currencyStr,_,amountStr,_,feeStr,_,status,_,_) = lines.take(16).toArray

        if(status=="Completed") {
          val txid = TxtUtils.parseTxid(txidStr)
          val date = TxtUtils.parseDate(dateStr)
          val currency = Currency.normalize(currencyStr)
          val amount = TxtUtils.parseAmount(amountStr)
          val feeAmount = TxtUtils.parseAmount(feeStr)

          val desc = RichText(s"Withdrawal ${RichText.util.transaction(currency, txid, address)}")

          val withdrawal = Withdrawal(
            date = date
            , id = txid
            , amount = amount
            , currency = currency
            , exchanger = Bittrex
            , description = desc
          )

          val fee =
            if(Config.config.fundingFees)
              Fee(
                date = date
                , id = txid
                , amount = feeAmount
                , currency = currency
                , exchanger = Bittrex
                , description = RichText(s"Bittrex withdrawal fee $currency $txid")
              )
            else
              NonTaxableFee(
                date = date
                , id = txid
                , amount = feeAmount
                , currency = currency
                , exchanger = Bittrex
                , description = RichText(s"Bittrex withdrawal non taxable fee $currency $txid")
              )

          operations += withdrawal
          operations += fee
        }
      }
      return operations.toList
    }
  }

  private def withdrawalsReader2(fileName: String) = new FileSource[Operation](fileName) {
    def withdrawalFee(currency: Currency, date: LocalDateTime): Double = {
      currency match {
        case Currency.bitcoin =>
          if(date.getYear<2017)
            0.0002
          else
            0.001
        case _ => 0
      }
    }

    def read(): Seq[Operation] = {
      val f = FileSystem.File(fileName)
      val sc = new java.util.Scanner(f)

      def getNextLine(): Option[String] = {
        while(sc.hasNextLine) {
          val ln = Parse.trimSpaces(sc.nextLine())
          if(ln.nonEmpty)
            return Some(ln)
        }
        return None
      }

      var operations = List[Operation]()

      getNextLine() // skip header

      var ok = true
      while(sc.hasNextLine) {
        val opts = (0 to 2).map(_ => getNextLine())

        ok = opts.forall(_.isDefined)

        if(ok) {
          val scLn = opts.map { case Some(ln) => SeparatedScanner(ln, "[ \t]+") }
          try {
            val date = LocalDateTime.parseAsMyZoneId(scLn(0).next()+" 00:00:00", "MM/dd/yyyy HH:mm:ss") // this is local time as it's taken from the GUI
            val currency = Currency.normalize(scLn(0).next())
            val amount = scLn(0).nextDouble()
            val status = scLn(0).next()

            scLn(1).next()
            val address = scLn(1).next()

            scLn(2).next()
            val txid = scLn(2).next()

            val paidFee = withdrawalFee(currency, date)
            if(status == "Completed" && paidFee > 0) {
              val desc = RichText(s"$id Withdrawal fee $currency $txid")

              val fee = Fee(
                date = date
                , id = desc.toString
                , amount = paidFee
                , currency = currency
                , exchanger = Bittrex
                , description = desc
              )
              operations ::= fee
            } else
              Logger.warning(s"$id. Read withdrawal ${FileSystem.pathFromData(fileName)}: This withdrawal was ignored: ${opts(0).get}.")
          } catch {
            case e => Logger.fatal(s"Something went wrong reading csv file $fileName. $e")
          } finally
            scLn.foreach(_.close())
        }
      }
      sc.close()
      return operations
    }
  }
}


/*

https://bitcointalk.org/index.php?topic=1970414.msg28376567#msg28376567

I just spent way too much time trying to figure out the answer to this question and I'm pretty sure I've got it.

1. Bittrex Fees are NOT always charged in the currency that is acquired.  Fees are charged in the currency listed first on the Currency.  So, fees for the USDT-BTC currency are charged in USDT, regardless of whether the transaction is a buy (Bid) or sell (Ask).

2. Cost/Proceeds are always expressed in the currency listed first on the Currency regardless whether it's a buy or sell.  If it's a buy, then the Cost/Proceeds will be negative since you are buying the Currency listed second in exchange for the Currency listed first.  Therefore, the currency listed first will be debited from one's wallet.  If it's a Sell then Cost/Proceeds will be positive, and thus credited to one's wallet.

2. Fees and Volumes for the currency listed first on the currency are both always truncated (truncated) after 8 decimal places.  My guess is that Bittrex does this so that it only ever needs to keep track of 8 decimal places for any currency held on its internal ledger, and therefore any currency volumes represented in an order (on the web page) are exactly as precise as the actual numbers.

3. Actual Rate differs from Bid/Ask rate because of this truncating....  At least this is one reason why the rate differs.  Presumably it could also differ because an order is filled at a more favorable rate than the Bid/Ask.

Here's an example:
Currency: USDT-ETH.  Transaction: LimitBuy.  853.553  Units Filled: 0.05748706  Units Total: 0.05748706  Actual/Rate: 853.55299992  Cost/Proceeds: -49.19092315
So I'm attempting to buy 0.05748706 ETH for USDT at a Rate of 853.553 USDT/ETH.  Since Fees are calculated in the currency listed first, the fees will be taken out of the USDT, which is the currency I'm starting with.  So I need to know how many dollars (X) will I need to exchange for 0.05748706 ETH if the Buy Rate is 853.553 USDT/ETH.  The answer is:  X = 0.05748706 ETH * 853.553 USDT/ETH = 49.06825252418 USDT.  But Bittrex truncates this number after 8 decimal places.  So Trunc(X) = 49.06825252 USDT

So I'm going to actually buy 0.05748706 ETH with 49.06825252 USDT NOT 49.06825252418 USDT and because of that, my actual rate will be different than my Bid rate.  My actual rate will therefore be: 49.06825252 USDT / 0.05748706 ETH = 853.552999927288 USDT / ETH, but Bittrex also truncates the Actual Rate after 8 decimal places, so my Actual Rate is 853.55299992 USDT / ETH, which exactly matches what is expected.

Since USDT is listed first, the fee calculation is taken in USDT, which means its an additional expenditure to the USDT amount that I used in my exchange/buy - Trunc(X) = 49.06825252 USDT.  So the Fee is:
0.0025*49.06825252 USDT = 0.1226706313 USDT, but the fee itself is also truncated after 8 decimal places, so the fee is 0.12267063 USDT

Finally, the Cost/Proceeds is calculated as the total USDT expended for the transaction plus fees: Cost/Proceeds = 49.06825252 USDT + 0.12267063 USDT = 49.19092315 USDT.  It's represented as a negative number because USDT is debited from my wallet.

Interestingly, this means that in this example, you need to start with enough USDT to pay both the fees and to exchange for ETH since the fees are taken out from the currency expended and not from the currency acquired.  I'm not sure what happens if you don't start with enough, but I would guess either Bittrex wouldn't let you do the trade or Bittrex would slightly decrease the amount of ETH you would acquire so that you would not need as much USDT to exchange and therefore would have some extra USDT to pay the fees.

___

A LimitSell would work similarly, but if we're selling ETH to acquire USDT then the fees would be deducted from the acquired USDT, since USDT is listed first on the USDT-ETH currency.  Again, the Actual Rate would differ from the Sell Rate due to truncating the amount of acquired USDT, and then recalculating the rate based on this truncating.

I've only traded on a few currencies to draw these conclusions, so someone with more experience on other Bittrex currencies might know otherwise, but this seems like what is going on.


 */