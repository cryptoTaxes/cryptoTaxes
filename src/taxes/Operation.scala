package taxes

import taxes.Exchanger.Exchanger
import taxes.Market.Market

trait Operation {
  def date : Date
  def id : String
  def exchanger : Exchanger

  protected val dateFormat = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss Z", java.util.Locale.ENGLISH)
  protected def dateToString(date : Date) : String = dateFormat.format(date)
}


object Operation {
  object OrderType extends Enumeration {
    val Buy, Sell = Value
  }
}


/*************************************************************************
  if(feeMarket == fromMarket)
     we exchanged ((fromAmount + fee))  for  ((toAmount))
  else if(feeMarket == toMarket)
     we exchanged ((fromAmount))  for  ((toAmount + fee))

  The fee is part of the exchange, i.e. it's not paid with other funds.
  So if feeMarket == toMarket you're only disposing fromAmount but
  part of these funds will be used to pay for the fee.
  If feeMarket == fromMarket you're really disposing (fromAmount + fee)
  ************************************************************************/
case class Exchange( date : Date
                    , id : String
                    , fromAmount : Double, fromMarket : Market
                    , toAmount : Double, toMarket : Market
                    , fee : Double, feeMarket : Market
                    , isSettlement : Boolean = false
                    , exchanger: Exchanger
                    , description : String
                    ) extends Operation {

  override def toString : String =
    "Exchange(%s %18.8f %-5s -> %18.8f %-5s  %18.8f %-5s  %s)" format (dateToString(date), fromAmount, fromMarket, toAmount, toMarket, fee, feeMarket, description)
}


/*************************************************************************
  NOTE THE LACK OF SYMMETRY:
  * Only toAmount (without fee, even if feeMarket == toMarket)
    will be added to your stock of coins
  * fromAmount (plus fee, if feeMarket == fromMarket)
    will be deducted from your stocks.
 ************************************************************************/
case class Margin( date : Date
                  , id : String
                  , fromAmount : Double, fromMarket : Market
                  , toAmount : Double, toMarket : Market
                  , fee : Double, feeMarket : Market
                  , orderType : Operation.OrderType.Value
                  , pair : (Market,Market)
                  , exchanger : Exchanger
                  , description : String
                  ) extends Operation {

  override def toString : String =
    "Margin(%s %18.8f %-5s -> %18.8f %-5s  %18.8f %-5s  %s)" format (dateToString(date), fromAmount, fromMarket, toAmount, toMarket, fee, feeMarket, description)
}


case class Fee(date : Date, id : String, amount: Double, market: Market, exchanger : Exchanger, description : String, alt : Option[(Double, Market)] = None) extends Operation {
  override def toString : String =
    "Fee(%s %18.8f %-5s  %s)" format (dateToString(date), amount, market, description)
}


case class Loss(date : Date, id : String, amount: Double, market: Market, exchanger : Exchanger, description : String) extends Operation {
  override def toString : String =
    "Lost(%s %18.8f %-5s  %s)" format (dateToString(date), amount, market, description)
}


// amount is the real gain (fee has already been deducted)
case class Gain(date : Date, id : String, amount: Double, market: Market, exchanger : Exchanger, description : String) extends Operation {
  override def toString : String =
    "Gain(%s %18.8f %-5s   %s)" format (dateToString(date), amount, market, description)
}


