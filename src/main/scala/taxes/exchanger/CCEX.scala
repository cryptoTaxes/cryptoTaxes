package taxes.exchanger

import taxes._
import taxes.date._
import taxes.io.FileSystem
import taxes.util.parse.{CSVReader, CSVSortedOperationReader, Scanner, SeparatedScanner}


object CCEX extends Exchanger {
  override val id: String = "C-CEX"

  override val sources = Seq(
    new FilteredUserInputFolderSource[Operation]("c-cex", ".csv") {
      def fileSource(fileName: String) = operationsReader(fileName)
    }
  )

  private def operationsReader(fileName: String) = new CSVSortedOperationReader(fileName) {
    override val linesToSkip = 1

    override def lineScanner(line: String) =
      SeparatedScanner(line, "[ \t]+")

    override def readLine(line: String, scLn: Scanner): CSVReader.Result[Operation] = {
      val token1 = scLn.next("Date1")
      val token2 = scLn.next("Date2")
      val orderType = scLn.next("Order Type")

      val date =
        if(Config.config.deprecatedUp2017Version)
          LocalDateTime.parseAsMyZoneId(s"$token1 $token2", "yyyy-MM-dd HH:mm:ss")
        else
          LocalDateTime.parseAsUTC(s"$token1 $token2", "yyyy-MM-dd HH:mm:ss") // C-CEX uses UTC

      if(orderType=="Transaction") {
        val toAmount = scLn.nextDouble("To Amount")
        val toCurrency = Currency.normalize(scLn.next("To Currency"))
        val fromAmount = scLn.nextDouble("From Amount")
        val fromCurrency = Currency.normalize(scLn.next("From Currency"))

        val feePercent = 0.2

        val fee = toAmount * feePercent / 100

        val exchange =
          Exchange(
            date = date
            , id = ""
            , fromAmount = fromAmount, fromCurrency = fromCurrency
            , toAmount = toAmount, toCurrency = toCurrency  // toAmount in csv doesn't include fee
            , fees = List(FeePair(fee, toCurrency))
            , exchanger = CCEX
            , description = RichText("")
          )

        return CSVReader.Ok(exchange)
      } else if(orderType=="Deposit") {
        val amount = scLn.nextDouble("Amount")
        val currency = Currency.normalize(scLn.next("Currency"))

        val skip1 = scLn.next("skip1")
        val skip2 = scLn.next("skip1")
        val skip3 = scLn.next("skip1")

        val txid = scLn.next("txid").init.tail

        val deposit = Deposit(
          date = date
          , id = ""
          , amount = amount
          , currency = currency
          , exchanger = CCEX
          , address = None
          , txid = Some(txid)
          , description = RichText("")
        )
        return CSVReader.Ok(deposit)
      } else if(orderType=="Withdrawal") {
        val skip1 = scLn.next("skip1")

        val amount = scLn.nextDouble("Amount")
        val currency = Currency.normalize(scLn.next("Currency"))

        val skip2 = scLn.next("skip2")
        val skip3 = scLn.next("skip3")
        val skip4 = scLn.next("skip4")

        val address = scLn.next("Address")
        val txid = scLn.next("txid").init.tail

        if(skip4 != "to") {
          val fee =
            if(Config.config.fundingFees)
              Fee(
                date = date
                , id = ""
                , amount = amount
                , currency = currency
                , exchanger = CCEX
                , description = RichText(s"C-CEXwithdrawal of ${Format.asCurrency(amount, currency)}")
              )
            else
              NonTaxableFee(
                date = date
                , id = ""
                , amount = amount
                , currency = currency
                , exchanger = CCEX
                , description = RichText(s"C-CEX withdrawal of ${Format.asCurrency(amount, currency)} non taxable fee")
              )
          return CSVReader.Ok(fee)
        } else {
          val withdrawal = Withdrawal(
            date = date
            , id = ""
            , amount = amount
            , currency = currency
            , exchanger = CCEX
            , address = Some(address)
            , txid = Some(txid)
            , description = RichText("")
          )
          return CSVReader.Ok(withdrawal)
        }
      } else
        return CSVReader.Warning(s"$id. Read file ${FileSystem.pathFromData(fileName)}: Reading this transaction is not currently supported: $line.")
    }
  }
}

