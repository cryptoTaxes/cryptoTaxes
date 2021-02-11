package taxes.exchanger

import taxes._
import taxes.date._
import taxes.io.FileSystem
import taxes.util._
import taxes.util.parse.{JsObjectAST, Parse}


object Shapeshift extends Exchanger {
  override val id: String = "Shapeshift"

  override val sources = Seq(
    new FilteredUserInputFolderSource[Operation]("shapeshift", ".json") {
      def fileSource(fileName: String) = new FileSource[Operation](fileName) {
        override def read(): Seq[Operation] =
          readFile(fileName)
      }
    }
  )

  def readFile(fileName: String): List[Operation] = {
    val prefix0 = "https://shapeshift.io/#/status/"
    val prefix1 = "https://shapeshift.io/txStat/"

    var lnNumber = 0
    val f = FileSystem.File(fileName)
    val sc = new java.util.Scanner(f, taxes.io.defaultCharset.name())

    def nextLine(): String = {
      lnNumber += 1
      Parse.trimSpaces(sc.nextLine())
    }

    var operations = List[Operation]()
    while(sc.hasNextLine) {
      val ln0 = nextLine()
      if(ln0.nonEmpty) {
        if(!ln0.startsWith(prefix0))
          Logger.warning(s"$id. ${FileSystem.pathFromData(fileName)} Line $lnNumber: '$ln0' should start with $prefix0.")
        else {
          val orderId = ln0.drop(prefix0.length)

          val ln1 = nextLine()
          if(!ln1.startsWith(prefix1))
            Logger.warning(s"$id. ${FileSystem.pathFromData(fileName)} Line $lnNumber: '$ln1' should start with $prefix1.")
          else {
            val inAddress = ln1.drop(prefix1.length)

            val ln2 = nextLine()
            val json = JsObjectAST.fromString(ln2)

            val depositAddress = json.getString("address")
            if(depositAddress != inAddress)
              Logger.warning(s"$id. ${FileSystem.pathFromData(fileName)} Line $lnNumber: Input address $depositAddress should be $inAddress.")
            else if(json.getString("status") != "complete")
              Logger.warning(s"$id. ${FileSystem.pathFromData(fileName)} Line $lnNumber: Status should be complete.")
            else {
              val fromAmount = json.getDouble("incomingCoin")
              val fromCurrency = Currency.normalize(json.getString("incomingType"))

              val toAmount = json.getDouble("outgoingCoin")
              val toCurrency = Currency.normalize(json.getString("outgoingType"))
              val withdrawalAddress = json.getString("withdraw")
              val toTxId = json.getString("transaction")

              val ln3 = nextLine()
              val fromTxId = ln3

              val fromTxInfo = TransactionsCache.lookup(fromCurrency, fromTxId, depositAddress)

              val date = fromTxInfo.localDate
              val orderUrl = s"https://shapeshift.io/orderInfo/$orderId"

              val deposit = Deposit(
                date = date
                , id = orderId
                , amount = fromAmount + fromTxInfo.fee
                , currency = fromCurrency
                , exchanger = Shapeshift
                , address = Some(depositAddress)
                , txid = Some(fromTxId)
                , description = RichText(RichText.url(orderId,orderUrl))
              )

              val exchUrl = RichText.util.onlineExchange(fromCurrency, fromTxId, toCurrency, toTxId)
              val desc = RichText(s"Order: ${RichText.url(orderId,orderUrl)} $exchUrl")

              val exchange =
                Exchange(
                  date = date
                  , id = orderId
                  , fromAmount = fromAmount, fromCurrency = fromCurrency
                  , toAmount = toAmount, toCurrency = toCurrency
                  , fees = List(FeePair(fromTxInfo.fee, fromCurrency))
                  , exchanger = Shapeshift
                  , description = desc
                )

              val withdrawal = Withdrawal(
                date = date
                , id = orderId
                , amount = toAmount
                , currency = toCurrency
                , exchanger = Shapeshift
                , address = Some(withdrawalAddress)
                , txid = Some(toTxId)
                , description = RichText(RichText.url(orderId,orderUrl))
              )

              operations ++= List(deposit, exchange, withdrawal)
            }
          }
        }
      }
    }
    sc.close()
    return operations.sortBy(_.date)
  }
}


