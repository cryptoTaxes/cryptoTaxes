package taxes.exchanger

import taxes._
import taxes.date._
import taxes.util.parse.{CSVReader, CSVSortedOperationReader, Scanner, SeparatedScanner}


object CCEX extends Exchanger {
  override val id: String = "C-CEX"

  override val sources = Seq(
    new UserFolderSource[Operation]("c-cex", ".csv") {
      def fileSource(fileName : String) = operationsReader(fileName)
    }
  )

  private def operationsReader(fileName : String) = new CSVSortedOperationReader(fileName) {
    override val linesToSkip = 1

    override def lineScanner(line: String) =
      SeparatedScanner(line, "[ \t]+")

    override def readLine(line: String, scLn: Scanner) : CSVReader.Result[Operation] = {
      val token1 = scLn.next("Date1")
      val token2 = scLn.next("Date2")
      val orderType = scLn.next("Order Type")

      if(orderType=="Transaction") {
        val date =
          if(Config.config.deprecatedUp2017Version)
            LocalDateTime.parseAsMyZoneId(s"$token1 $token2", "yyyy-MM-dd HH:mm:ss")
          else
            LocalDateTime.parseAsUTC(s"$token1 $token2", "yyyy-MM-dd HH:mm:ss") // C-CEX uses UTC

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
      } else
        return CSVReader.Warning(s"$id. Read file ${FileSystem.pathFromData(fileName)}: Reading this transaction is not currently supported: $line.")
    }
  }
}

