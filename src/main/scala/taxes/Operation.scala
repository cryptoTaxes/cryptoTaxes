package taxes

import taxes.date._
import taxes.exchanger.Exchanger

import spray.json._
import spray.json.JsonProtocol._


sealed trait Operation {
  def date: LocalDateTime
  def id: String
  def exchanger: Exchanger
  def description: String

  protected def dateFormatted =
    java.time.format.DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss").format(date)

  protected def shortDescription = {
    val maxLen = 35
    val suffix = "..."
    val str = util.parse.Parse.trimSpaces(description.replace("\n", " "))
    if(str.length > maxLen)
      str.take(maxLen-suffix.length) ++ suffix
    else
      str
  }
}


object Operation {
  def feesToString(fees: Seq[FeePair]): String =
    fees.map(fee => f"${fee.amount}%.8f ${fee.market}").mkString("(", ", ", ")")

  object OrderType extends Enumeration {
    val Buy, Sell = Value
  }

  implicit val orderJson = jsonEnumFormat(OrderType)
  implicit val feePairJson = jsonFormat3(FeePair)
  implicit val exchangeJson = jsonFormat10(Exchange)
  implicit val marginJson = jsonFormat11(Margin)
  implicit val feeJson = jsonFormat7(Fee)
  implicit val feeJLoss = jsonFormat6(Loss)
  implicit val gainJson = jsonFormat6(Gain)
  implicit val depositJson = jsonFormat6(Deposit)
  implicit val withdrawalJson = jsonFormat6(Withdrawal)
  implicit val nonTaxableFeeJson = jsonFormat6(NonTaxableFee)

  implicit object operationJson extends RootJsonFormat[Operation] {
    val _Exchange = "Exchange"
    val _Margin = "Margin"
    val _Fee = "Fee"
    val _Loss = "Loss"
    val _Gain = "Gain"
    val _Deposit = "Deposit"
    val _Withdrawal = "Withdrawal"
    val _NonTaxableFee = "NonTaxableFee"

    val _type = "type"
    val _operation = "operation"

    def write(operation: Operation) = {
      val (tag, json) = operation match {
        case e: Exchange => (_Exchange, e.toJson)
        case m: Margin => (_Margin, m.toJson)
        case f: Fee => (_Fee, f.toJson)
        case l: Loss => (_Loss, l.toJson)
        case g: Gain => (_Gain, g.toJson)
        case d: Deposit => (_Deposit, d.toJson)
        case w: Withdrawal => (_Withdrawal, w.toJson)
        case ntf: NonTaxableFee => (_NonTaxableFee, ntf.toJson)
      }
      JsObject(_type -> JsString(tag), _operation -> json)
    }

    def read(value: JsValue): Operation =
      try {
        value.asJsObject.getFields(_type, _operation) match {
          case Seq(JsString(tag), jsObj) =>
            tag match {
              case `_Exchange` => jsObj.convertTo[Exchange]
              case `_Margin` => jsObj.convertTo[Margin]
              case `_Fee` => jsObj.convertTo[Fee]
              case `_Loss` => jsObj.convertTo[Loss]
              case `_Gain` => jsObj.convertTo[Gain]
              case `_Deposit` => jsObj.convertTo[Deposit]
              case `_Withdrawal` => jsObj.convertTo[Withdrawal]
              case `_NonTaxableFee` => jsObj.convertTo[NonTaxableFee]
            }
        }
      } catch {
            case _ => deserializationError(s"Operation expected in $value")
      }
  }


}


/*************************************************************************

      if(feeMarket == fromMarket)
         we exchanged ((fromAmount + feeAmount))  for  ((toAmount))
      else if(feeMarket == toMarket)
         we exchanged ((fromAmount))  for  ((toAmount + feeAmount))

      The fee is part of the exchange, i.e. it's not paid with other funds.

      So if feeMarket == fromMarket you're really disposing (fromAmount + feeAmount)
      from your balance of fromMarket. Note that, in this case, fromAmount is what
      you pay but without including the fee. toAmount is what you end up adding to
      your balance of toMarket.

      If feeMarket == toMarket you're only disposing fromAmount from your fromMarket balance
      but part of these funds, after being exchanged, will be used to pay for the fee.
      Note that, in this case, toAmount is not all you get after the exchange, but
      what you really end up maintaining in your toMarket balance after paying the fee.


  For the case of a detached fee, the corresponding fee amount is not part of the exchange hence
      we exchanged ((fromAmount)) for ((toAmount))
      and independently pay the fee from other funds
  ************************************************************************/

case class FeePair(amount: Double, market: Market, alt: Option[(Double, Market)] = None)


case class Exchange( date: LocalDateTime
                   , id: String
                   , fromAmount: Double, fromMarket: Market
                   , toAmount: Double, toMarket: Market
                   , fees: Seq[FeePair]
                   , isSettlement: Boolean = false
                   , exchanger: Exchanger
                   , description: String
                   ) extends Operation {

  override def toString: String =
    f"Exchange($dateFormatted $fromAmount%18.8f $fromMarket%-5s -> $toAmount%18.8f $toMarket%-5s  ${Operation.feesToString(fees)}  $shortDescription)"
}



/*************************************************************************
  * Contrary to exchange, fee is not part of the exchange rate.
  *
  * Only toAmount (without fee, even if feeMarket == toMarket)
    will be added to your stock of coins
  * fromAmount (without fee, if feeMarket == fromMarket)
    will be deducted from your stock.
 ************************************************************************/
case class Margin(date: LocalDateTime
                  , id: String
                  , fromAmount: Double, fromMarket: Market
                  , toAmount: Double, toMarket: Market
                  , fees: Seq[FeePair]
                  , orderType: Operation.OrderType.Value
                  , pair: (Market, Market)
                  , exchanger: Exchanger
                  , description: String
                  ) extends Operation {

  override def toString: String =
    f"Margin($dateFormatted $fromAmount%18.8f $fromMarket%-5s -> $toAmount%18.8f $toMarket%-5s ${Operation.feesToString(fees)} $shortDescription)"
}


case class Fee(date: LocalDateTime, id: String, amount: Double, market: Market, exchanger: Exchanger, description: String, alt: Option[(Double, Market)] = None) extends Operation {
  override def toString: String =
    f"Fee($dateFormatted $amount%18.8f $market%-5s $shortDescription)"
}


case class Loss(date: LocalDateTime, id: String, amount: Double, market: Market, exchanger: Exchanger, description: String) extends Operation {
  override def toString: String =
    f"Lost($dateFormatted $amount%18.8f $market%-5s $shortDescription)"
}


// amount is the real gain (fee has already been deducted)
case class Gain(date: LocalDateTime, id: String, amount: Double, market: Market, exchanger: Exchanger, description: String) extends Operation {
  override def toString: String =
    f"Gain($dateFormatted $amount%18.8f $market%-5s $shortDescription)"
}


case class Deposit(date: LocalDateTime, id: String, amount: Double, market: Market, exchanger: Exchanger, description: String) extends Operation {
  override def toString: String =
    f"Deposit($dateFormatted $amount%18.8f $market%-5s $shortDescription)"
}

case class Withdrawal(date: LocalDateTime, id: String, amount: Double, market: Market, exchanger: Exchanger, description: String) extends Operation {
  override def toString: String =
    f"Withdrawal($dateFormatted $amount%18.8f $market%-5s $shortDescription)"
}

case class NonTaxableFee(date: LocalDateTime, id: String, amount: Double, market: Market, exchanger: Exchanger, description: String) extends Operation {
  override def toString: String =
    f"NonTaxableFee($dateFormatted $amount%18.8f $market%-5s $shortDescription)"
}