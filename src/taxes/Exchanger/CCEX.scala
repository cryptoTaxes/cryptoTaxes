package taxes.Exchanger

import taxes.Util.Parse.{CSVSortedOperationReader, Scanner, SeparatedScanner}
import taxes._

object CCEX extends Exchanger {

  override val id: String = "C-CEX"

  override val sources = Seq(
    new UserFolderSource[Operation]("c-cex") {
      def fileSource(fileName : String) = operationsReader(fileName)
    }
  )

  private def operationsReader(fileName : String) = new CSVSortedOperationReader(fileName) {
    override val hasHeader: Boolean = true

    override def lineScanner(line: String) =
      SeparatedScanner(line, "[ \t]+")

    override def readLine(line: String, scLn: Scanner) : Either[String,Operation] = {
      val token1 = scLn.next()
      val token2 = scLn.next()
      val orderType = scLn.next()

      if(orderType=="Transaction") {
        val date = Date.fromString(token1+" "+token2, "yyyy-MM-dd hh:mm:ss")
        val amount1 = scLn.nextDouble()
        val market1 = scLn.next()
        val amount2 = scLn.nextDouble()
        val market2 = scLn.next()
        scLn.close()

        val feePercent = 0.2

        val exchange =
          Exchange(
            date = date
            , id = ""
            , fromAmount = amount2, fromMarket = Market.normalize(market2)
            , toAmount = amount1, toMarket = Market.normalize(market1)
            , fee = // state fee in BTC
              if (market1=="BTC")
                amount1 * feePercent / 100
              else
                amount2 / (1 + 100/feePercent)

            , feeMarket = Market.bitcoin
            , exchanger = CCEX
            , description = id
          )

        return Right(exchange)
      } else
        return Left("%s. Read file. Reading this transaction is not currently supported: %s.".format(id, line))
    }
  }
}

