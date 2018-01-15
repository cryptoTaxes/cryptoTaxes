package taxes

import taxes.Exchanger.Exchanger
import taxes.Market.Market

trait Operation {
  def date : Date
  def id : String
  def exchanger : Exchanger
  def description : String

  protected val dateFormat = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss Z", java.util.Locale.ENGLISH)
  protected def dateToString(date : Date) : String = dateFormat.format(date)
}


object Operation {
  object OrderType extends Enumeration {
    val Buy, Sell = Value
  }
}


// fromAmount is total released amount
// toAmount is total received amount (real quantity without what was taken by exchanger as a fee)
// fee is absolute fee taken by exchanger
case class Exchange( date : Date
                    , id : String
                    , fromAmount : Double, fromMarket : Market
                    , toAmount : Double, toMarket : Market
                    , fee : Double, feeMarket : Market
                    , exchanger: Exchanger
                    , description : String
                    ) extends Operation {

  override def toString : String =
    "Exchange(%s %18.8f %-5s -> %18.8f %-5s  %18.8f %-5s  %s)" format (dateToString(date), fromAmount, fromMarket, toAmount, toMarket, fee, feeMarket, description)
}


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


case class SettlementBuy( date : Date
                         , id : String
                         , fromAmount : Double, fromMarket : Market
                         , toAmount : Double, toMarket : Market
                         , fee : Double, feeMarket : Market
                         , exchanger: Exchanger
                         , description : String
                         ) extends Operation {

  override def toString : String =
    "SettlementBuy(%s %18.8f %-5s -> %18.8f %-5s  %18.8f %-5s  %s)" format (dateToString(date), fromAmount, fromMarket, toAmount, toMarket, fee, feeMarket, description)
}


case class Fee(date : Date, id : String, amount: Double, market: Market, exchanger : Exchanger, description : String) extends Operation {
  override def toString : String =
    "Fee(%s %18.8f %-5s  %s)" format (dateToString(date), amount, market, description)
}


// amount is without taking fee into account (fee should be added to amount when Loss is processed)
case class Loss(date : Date, id : String, amount: Double, market: Market, fee: Double, feeMarket : Market, exchanger : Exchanger, description : String) extends Operation {
  override def toString : String =
    "Lost(%s %18.8f %-5s  %18.8f %-5s  %s)" format (dateToString(date), amount, market, fee, feeMarket, description)
}


// amount is the real gain (fee has already been deducted)
case class Gain(date : Date, id : String, amount: Double, market: Market, fee: Double, feeMarket : Market, exchanger : Exchanger, description : String) extends Operation {
  override def toString : String =
    "Gain(%s %18.8f %-5s   %18.8f %-5s  %s)" format (dateToString(date), amount, market, fee, feeMarket, description)
}