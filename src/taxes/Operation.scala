package taxes

import taxes.date._
import taxes.exchanger.Exchanger


trait Operation {
  def date : LocalDateTime
  def id : String
  def exchanger : Exchanger

  protected def dateFormatted =
    java.time.format.DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss").format(date)
}


object Operation {
  object OrderType extends Enumeration {
    val Buy, Sell = Value
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

case class FeePair(amount : Double, market: Market)


case class Exchange(date : LocalDateTime
                    , id : String
                    , fromAmount : Double, fromMarket : Market
                    , toAmount : Double, toMarket : Market
                    , fees : Seq[FeePair]
                    , isSettlement : Boolean = false
                    , exchanger: Exchanger
                    , description : String
                    ) extends Operation {

  override def toString : String = {
    val feesStr = fees.map(fee => f"${fee.amount}%.8f ${fee.market}").mkString("(", ", ", ")")
    f"Exchange($dateFormatted $fromAmount%18.8f $fromMarket%-5s -> $toAmount%18.8f $toMarket%-5s  $feesStr  $description)"
  }
}


/*************************************************************************
  * Contrary to exchange, fee is not part of the exchange rate.
  *
  * Only toAmount (without fee, even if feeMarket == toMarket)
    will be added to your stock of coins
  * fromAmount (without fee, if feeMarket == fromMarket)
    will be deducted from your stock.
 ************************************************************************/
case class Margin(date : LocalDateTime
                  , id : String
                  , fromAmount : Double, fromMarket : Market
                  , toAmount : Double, toMarket : Market
                  , feeAmount : Double, feeMarket : Market
                  , orderType : Operation.OrderType.Value
                  , pair : (Market, Market)
                  , exchanger : Exchanger
                  , description : String
                  ) extends Operation {

  override def toString : String =
    f"Margin($dateFormatted $fromAmount%18.8f $fromMarket%-5s -> $toAmount%18.8f $toMarket%-5s  $feeAmount%18.8f $feeMarket%-5s  $description)"
}


case class Fee(date : LocalDateTime, id : String, amount: Double, market: Market, exchanger : Exchanger, description : String, alt : Option[(Double, Market)] = None) extends Operation {
  override def toString : String =
    f"Fee($dateFormatted $amount%18.8f $market%-5s  $description)"
}


case class Loss(date : LocalDateTime, id : String, amount: Double, market: Market, exchanger : Exchanger, description : String) extends Operation {
  override def toString : String =
    f"Lost($dateFormatted $amount%18.8f $market%-5s  $description)"
}


// amount is the real gain (fee has already been deducted)
case class Gain(date : LocalDateTime, id : String, amount: Double, market: Market, exchanger : Exchanger, description : String) extends Operation {
  override def toString : String =
    f"Gain($dateFormatted $amount%18.8f $market%-5s   $description)"
}


