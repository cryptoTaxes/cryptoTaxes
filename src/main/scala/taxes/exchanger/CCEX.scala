package taxes.exchanger

import taxes._
import taxes.date._
import taxes.io.FileSystem
import taxes.util.parse.{CSVReader, CSVSortedOperationReader, Scanner, SeparatedScanner}


object CCEX extends Exchanger {
  override val id: String = "C-CEX"

  override val sources = Seq(
    new UserInputFolderSource[Operation]("c-cex", ".csv") {
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
        val toMarket = Market.normalize(scLn.next("To Market"))
        val fromAmount = scLn.nextDouble("From Amount")
        val fromMarket = Market.normalize(scLn.next("From Market"))

        val feePercent = 0.2

        val fee = toAmount * feePercent / 100

        val exchange =
          Exchange(
            date = date
            , id = ""
            , fromAmount = fromAmount, fromMarket = fromMarket
            , toAmount = toAmount, toMarket = toMarket  // toAmount in csv doesn't include fee
            , fees = List(FeePair(fee, toMarket))
            , exchanger = CCEX
            , description = ""
          )

        return CSVReader.Ok(exchange)
      } else if(orderType=="Deposit") {
        val amount = scLn.nextDouble("Amount")
        val market = Market.normalize(scLn.next("Market"))

        val skip1 = scLn.next("skip1")
        val skip2 = scLn.next("skip1")
        val skip3 = scLn.next("skip1")

        val txid = scLn.next("txid")


        val desc = "Deposit " + txid
        val deposit = Deposit(
          date = date
          , id = txid
          , amount = amount
          , market = market
          , exchanger = CCEX
          , description = desc
        )
        return CSVReader.Ok(deposit)
      } else if(orderType=="Withdrawal") {
        val skip1 = scLn.next("skip1")

        val amount = scLn.nextDouble("Amount")
        val market = Market.normalize(scLn.next("Market"))

        val skip2 = scLn.next("skip2")
        val skip3 = scLn.next("skip3")
        val skip4 = scLn.next("skip4")

        val address = scLn.next("Address")
        val txid = scLn.next("txid")

        val desc = "Withdrawal " + AddressBook.format(address) + "\n" + txid
        val withdrawal = Withdrawal(
          date = date
          , id = txid
          , amount = amount
          , market = market
          , exchanger = CCEX
          , description = desc
        )
        return CSVReader.Ok(withdrawal)
      } else
        return CSVReader.Warning(s"$id. Read file ${FileSystem.pathFromData(fileName)}: Reading this transaction is not currently supported: $line.")
    }
  }
}

