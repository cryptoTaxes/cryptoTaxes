package taxes

import taxes.date._
import taxes.exchanger.Exchanger


trait Operation {
  def date : LocalDateTime
  def id : String
  def exchanger : Exchanger

  def dateFormatted =
    java.time.format.DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss").format(date)
}


object Operation {
  object OrderType extends Enumeration {
    val Buy, Sell = Value
  }
}


/*************************************************************************
  if isDetachedFee is false and fee is expressed
  in same currency as fromMarket or toMarket:

      if(feeMarket == fromMarket)
         we exchanged ((fromAmount + feeAmount))  for  ((toAmount))
      else if(feeMarket == toMarket)
         we exchanged ((fromAmount))  for  ((toAmount + feeAmount))

      The fee is part of the exchange, i.e. it's not paid with other funds.
      So if feeMarket == toMarket you're only disposing fromAmount but
      part of these funds will be used to pay for the fee.
      If feeMarket == fromMarket you're really disposing (fromAmount + feeAmount)

  otherwise, feeAmount is not part of the exchange hence
      we exchanged ((fromAmount)) for ((toAmount))
  ************************************************************************/
case class Exchange(date : LocalDateTime
                    , id : String
                    , fromAmount : Double, fromMarket : Market
                    , toAmount : Double, toMarket : Market
                    , feeAmount : Double, feeMarket : Market
                    , detachedFee : Option[(Double, Market)] = None
                    , isSettlement : Boolean = false
                    , exchanger: Exchanger
                    , description : String
                    ) extends Operation {

  override def toString : String =
    "Exchange(%s %18.8f %-5s -> %18.8f %-5s  %18.8f %-5s  %s)" format (dateFormatted, fromAmount, fromMarket, toAmount, toMarket, feeAmount, feeMarket, description)
}


/*************************************************************************
  * Contrary to exchange, fee is not part of the exchange rate.
  *
  * Only toAmount (without fee, even if feeMarket == toMarket)
    will be added to your stock of coins
  * fromAmount (without fee, if feeMarket == fromMarket)
    will be deducted from your stock.
 ************************************************************************/
case class Margin( date : LocalDateTime
                  , id : String
                  , fromAmount : Double, fromMarket : Market
                  , toAmount : Double, toMarket : Market
                  , fee : Double, feeMarket : Market
                  , orderType : Operation.OrderType.Value
                  , pair : (Market, Market)
                  , exchanger : Exchanger
                  , description : String
                  ) extends Operation {

  override def toString : String =
    "Margin(%s %18.8f %-5s -> %18.8f %-5s  %18.8f %-5s  %s)" format (dateFormatted, fromAmount, fromMarket, toAmount, toMarket, fee, feeMarket, description)
}


case class Fee(date : LocalDateTime, id : String, amount: Double, market: Market, exchanger : Exchanger, description : String, alt : Option[(Double, Market)] = None) extends Operation {
  override def toString : String =
    "Fee(%s %18.8f %-5s  %s)" format (dateFormatted, amount, market, description)
}


case class Loss(date : LocalDateTime, id : String, amount: Double, market: Market, exchanger : Exchanger, description : String) extends Operation {
  override def toString : String =
    "Lost(%s %18.8f %-5s  %s)" format (dateFormatted, amount, market, description)
}


// amount is the real gain (fee has already been deducted)
case class Gain(date : LocalDateTime, id : String, amount: Double, market: Market, exchanger : Exchanger, description : String) extends Operation {
  override def toString : String =
    "Gain(%s %18.8f %-5s   %s)" format (dateFormatted, amount, market, description)
}


