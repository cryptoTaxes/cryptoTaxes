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
    override val linesToSkip = 1

    override val charSet: String = "UTF-16LE"

    override def lineScanner(line: String) =
      SeparatedScanner(line, "[,]")

    override def readLine(line: String, scLn: Scanner): CSVReader.Result[Operation] = {
      val orderId = scLn.next("Order ID")
      val (m1, aux) = scLn.next("Pair").span(_ != '-')
      val m2 = aux.tail

      val market1 = Market.normalize(m1)
      val market2 = Market.normalize(m2)

      val isSell = scLn.next("Order Type") == "LIMIT_SELL"
      val quantity = scLn.nextDouble("Quantity")
      val limit = scLn.nextDouble("Limit")
      val comissionPaid = scLn.nextDouble("Comission Paid")

      val price = scLn.nextDouble("Price")

      val dateOpen = Date.fromString(scLn.next("Open Date") + " +0000", "MM/dd/yyyy hh:mm:ss a Z") // Bittrex time is 1 hour behind here
      val dateClose = Date.fromString(scLn.next("Close Date") + " +0000", "MM/dd/yyyy hh:mm:ss a Z")

      val desc = "Order: " + orderId

      // market1 is normally BTC, USDT or ETH.
      // fees are denominated in market1.
      // Rate is computed as Price / Quantity (here Price stands really for what you're buying or selling)
      // In a sell you release Quantity coins. You get Price minus comissionPaid coins
      // A buy gets you Quantity coins. You pay Price but Price doesn't include comissionPaid

      val exchange =
        if (isSell)
          Exchange(
            date = dateClose
            , id = orderId
            , fromAmount = quantity, fromMarket = market2
            , toAmount = price - comissionPaid, toMarket = market1
            , fee = comissionPaid, feeMarket = market1
            , exchanger = Bittrex
            , description = desc
          )
        else
          Exchange(
            date = dateClose
            , id = orderId
            , fromAmount = price, fromMarket = market1
            , toAmount = quantity, toMarket = market2
            , fee = comissionPaid, feeMarket = market1
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
              Logger.warning("%s. Read withdrawal %s: This withdrawal was ignored: %s.".format(id, Paths.pathFromData(fileName), opts(0).get))
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


/*

https://bitcointalk.org/index.php?topic=1970414.msg28376567#msg28376567

I just spent way too much time trying to figure out the answer to this question and I'm pretty sure I've got it.

1. Bittrex Fees are NOT always charged in the currency that is acquired.  Fees are charged in the currency listed first on the Market.  So, fees for the USDT-BTC market are charged in USDT, regardless of whether the transaction is a buy (Bid) or sell (Ask).

2. Cost/Proceeds are always expressed in the currency listed first on the Market regardless whether it's a buy or sell.  If it's a buy, then the Cost/Proceeds will be negative since you are buying the Currency listed second in exchange for the Currency listed first.  Therefore, the currency listed first will be debited from one's wallet.  If it's a Sell then Cost/Proceeds will be positive, and thus credited to one's wallet.

2. Fees and Volumes for the currency listed first on the market are both always truncated (truncated) after 8 decimal places.  My guess is that Bittrex does this so that it only ever needs to keep track of 8 decimal places for any currency held on its internal ledger, and therefore any currency volumes represented in an order (on the web page) are exactly as precise as the actual numbers.

3. Actual Rate differs from Bid/Ask rate because of this truncating....  At least this is one reason why the rate differs.  Presumably it could also differ because an order is filled at a more favorable rate than the Bid/Ask.

Here's an example:
Market: USDT-ETH.  Transaction: LimitBuy.  853.553  Units Filled: 0.05748706  Units Total: 0.05748706  Actual/Rate: 853.55299992  Cost/Proceeds: -49.19092315
So I'm attempting to buy 0.05748706 ETH for USDT at a Rate of 853.553 USDT/ETH.  Since Fees are calculated in the market listed first, the fees will be taken out of the USDT, which is the currency I'm starting with.  So I need to know how many dollars (X) will I need to exchange for 0.05748706 ETH if the Buy Rate is 853.553 USDT/ETH.  The answer is:  X = 0.05748706 ETH * 853.553 USDT/ETH = 49.06825252418 USDT.  But Bittrex truncates this number after 8 decimal places.  So Trunc(X) = 49.06825252 USDT

So I'm going to actually buy 0.05748706 ETH with 49.06825252 USDT NOT 49.06825252418 USDT and because of that, my actual rate will be different than my Bid rate.  My actual rate will therefore be: 49.06825252 USDT / 0.05748706 ETH = 853.552999927288 USDT / ETH, but Bittrex also truncates the Actual Rate after 8 decimal places, so my Actual Rate is 853.55299992 USDT / ETH, which exactly matches what is expected.

Since USDT is listed first, the fee calculation is taken in USDT, which means its an additional expenditure to the USDT amount that I used in my exchange/buy - Trunc(X) = 49.06825252 USDT.  So the Fee is:
0.0025*49.06825252 USDT = 0.1226706313 USDT, but the fee itself is also truncated after 8 decimal places, so the fee is 0.12267063 USDT

Finally, the Cost/Proceeds is calculated as the total USDT expended for the transaction plus fees: Cost/Proceeds = 49.06825252 USDT + 0.12267063 USDT = 49.19092315 USDT.  It's represented as a negative number because USDT is debited from my wallet.

Interestingly, this means that in this example, you need to start with enough USDT to pay both the fees and to exchange for ETH since the fees are taken out from the currency expended and not from the currency acquired.  I'm not sure what happens if you don't start with enough, but I would guess either Bittrex wouldn't let you do the trade or Bittrex would slightly decrease the amount of ETH you would acquire so that you would not need as much USDT to exchange and therefore would have some extra USDT to pay the fees.

___

A LimitSell would work similarly, but if we're selling ETH to acquire USDT then the fees would be deducted from the acquired USDT, since USDT is listed first on the USDT-ETH market.  Again, the Actual Rate would differ from the Sell Rate due to truncating the amount of acquired USDT, and then recalculating the rate based on this truncating.

I've only traded on a few markets to draw these conclusions, so someone with more experience on other Bittrex markets might know otherwise, but this seems like what is going on.


 */