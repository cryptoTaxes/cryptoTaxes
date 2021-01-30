package taxes.exchanger

import taxes._
import taxes.date._
import taxes.util.parse._


object Kucoin extends Exchanger {
  override val id: String = "Kucoin"

  override val sources = Seq(
    new FilteredUserInputFolderSource[Operation]("kucoin", ".csv") {
      def fileSource(fileName: String) = operationsReader(fileName)
    }
  )

  private def operationsReader(fileName: String) = new CSVSortedOperationReader(fileName) {
    override val linesToSkip = 1

    override def lineScanner(line: String): Scanner =
      QuotedScanner(line, '\"', ',')

    override def readLine(line: String, scLn: Scanner): CSVReader.Result[Operation] = {
      val oid = scLn.next("oid")
      val symbol = scLn.next("symbol")
      val dealPrice = scLn.nextDouble("dealPrice")
      val dealValue = scLn.nextDouble("dealValue")
      val amount = scLn.nextDouble("amount")
      val fee = scLn.nextDouble("fee")
      val direction = scLn.next("direction")

      val createdDate = LocalDateTime.parseAsUTC(scLn.next("createdDate"), "+0800", "yyyy-MM-dd HH:mm:ss.S") // Kucoin trade history file seems to use UTC+8 time zone

      val (_baseCurrency, _quoteCurrency) = Parse.split(symbol, "-")
      val baseCurrency = Currency.normalize(_baseCurrency)
      val quoteCurrency = Currency.normalize(_quoteCurrency)

      val desc = RichText(s"Order: $oid")

      /* According to question 13 in
         https://www.reddit.com/r/kucoin/comments/7dm6s5/new_to_kucoin_check_this_out_faq/
         Q13: Where are the trade fees deducted? Is there a list?
         A: The trade fee is deducted from the token you bought or sold. For example: If
         you sell 100KCS, the system will deduct 1KCS as the trade fee, you actually only
         sold 99 KCS. If you bought 100KCS, the system will deduct 1KCS as the trade fee,
         you will receive only 99 KCS.

         For trade fee, we do not list them separately.
       */

      if(direction == "BUY") {
        val exchange = Exchange(
          date = createdDate
          , id = oid
          , fromAmount = dealValue, fromCurrency = quoteCurrency
          , toAmount = amount - fee, toCurrency = baseCurrency
          , fees = List(FeePair(fee, baseCurrency))
          , exchanger = Kucoin
          , description = desc
        )
        return CSVReader.Ok(exchange)
      } else {
        // toDo we need an example of a sell operation to check this
        val exchange = Exchange(
          date = createdDate
          , id = oid
          , fromAmount = amount - fee, fromCurrency = baseCurrency
          , toAmount = dealValue, toCurrency = quoteCurrency
          , fees = List(FeePair(fee, baseCurrency))
          , exchanger = Kucoin
          , description = desc
        )
        return CSVReader.Ok(exchange)
      }
    }
  }
}
