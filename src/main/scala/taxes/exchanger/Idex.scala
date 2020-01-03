package taxes.exchanger

import taxes._
import taxes.date._
import taxes.io.FileSystem
import taxes.util.Logger
import taxes.util.parse._


object Idex extends Exchanger {
  override val id: String = "Idex"

  override val sources = Seq(
    new UserInputFolderSource[Operation]("idex", ".csv") {
      def fileSource(fileName: String) = operationsReader(fileName)
    }
  )

  private def operationsReader(fileName: String) = new CSVSortedOperationReader(fileName) {
    override val linesToSkip = 1

    override def lineScanner(line: String): Scanner =
      QuotedScanner(line, '\"', ',')

    override def readLine(line: String, scLn: Scanner): CSVReader.Result[Operation] = {
      val transactionId = scLn.next("transactionId")
      val transactionHash = scLn.next("transactionHash")
      val date = LocalDateTime.parseAsUTC(scLn.next("date"), "+0100", "yyyy-MM-dd HH:mm:ss") // Idex trade history file seems to use UTC+1 time zone
      val currency = scLn.next("market")
      val makerOrTaker = scLn.next("makerOrTaker")
      val buyOrSell = scLn.next("buyOrSell")
      val tokenAmount = scLn.nextDouble("tokenAmount")
      val etherAmount = scLn.nextDouble("etherAmount")
      val usdValue = scLn.nextDouble("usdValue")
      val fee = scLn.nextDouble("fee")
      val gasFee = scLn.nextDouble("gasFee")


      val (_baseCurrency, _quoteCurrency) = Parse.split(currency, "/")
      val baseCurrency = Currency.normalize(_baseCurrency)
      val quoteCurrency = Currency.normalize(_quoteCurrency)

      val desc = RichText(s"Order: $transactionId $transactionHash")

      /*
      https://idex.currency/faq

      What are the fees to trade on IDEX?

      IDEX charges 0.2% for the currency taker and 0.1% for the currency maker.
      Users also pay gas fees to put their transactions on blockchain (read more below).

      What is a maker and taker?

      Currency makers place new orders on the books and wait for another user to match them.
      They can be buys or sells. Currency takers find existing orders on the books and fill
      them, thus taking orders off of the books.

      I made a trade as a currency taker, why was I charged more than 0.2%?

      Currency takers are responsible for covering the gas fees associated with each trade.
      Given our design, the exchange must pay this gas fee, priced in ether, when dispatching
      the trade to the network and then deduct it from the balance of the currency taker. When
      exchanging tokens for ether the amount of eth deducted matches that of the gas fee. When
      exchanging ether for tokens, IDEX deducts the equivalent amount of tokens based on the
      price of the asset in ETH. This price is calculated using the average of the last 10 trades.

      Ethereum gas prices have been increasing and often this fee is higher than the IDEX
      exchange fee of 0.2%. These high gas prices have led us to instituting order minimums
      in an attempt to reduce costs for our users (see question below on trade minimums).

      What are gas fees?

      All transactions on the Ethereum network cost gas, a fee that is paid to miners in order
      to process the transaction. Trades on IDEX cost ~140k gas. Gas costs are about 1.5x higher
      than EtherDelta and slightly higher than 0x, but on IDEX there is no risk of competing
      for the same order and wasting this gas fee. Traders receive their chosen order regardless
      of how long the transaction takes to settle, so as the exchange grows IDEX can continue
      to use a normal gas price.

      Who pays the gas fees?

      Users are responsible for covering all gas fees required to use the exchange. Deposit
      gas fees are paid directly by the user. On all other transactions and withdrawals,
      IDEX pays the gas fee to the network and deducts this fee from the user’s transaction.
      This includes both gas fees for trades, paid for by the taker, and gas fees for withdrawals,
      paid for by the customer. For trades and withdrawals the fee is taken out of the asset being
      traded/withdrawn. When withdrawing tokens the fee amount is calculated using the Token/ETH
      exchange rate from the last 10 trades.
      */

      if(buyOrSell == "buy") {
        val gasFeeInBaseCurrency =
          if(quoteCurrency != Currency.ethereum) {
            Logger.warning(s"$id. Read file ${FileSystem.pathFromData(fileName)}: Reading gass fee for this transaction is not currently supported: $line.")
            0
          } else {
            // we don't know prices for last 10 trades so we just use current one
            tokenAmount * gasFee / etherAmount
          }

        val exchange = Exchange(
          date = date
          , id = transactionId
          , fromAmount = etherAmount, fromCurrency = quoteCurrency
          , toAmount = tokenAmount - fee - gasFeeInBaseCurrency, toCurrency = baseCurrency
          , fees = List(FeePair(fee, baseCurrency), FeePair(gasFeeInBaseCurrency, baseCurrency, alt = Some(gasFee, quoteCurrency)))
          , exchanger = Idex
          , description = desc
        )
        return CSVReader.Ok(exchange)
      } else
        return CSVReader.Warning(s"$id. Read file ${FileSystem.pathFromData(fileName)}: Reading this transaction is not currently supported: $line.")
    }
  }
}
