package taxes

import taxes.Exchanger.Exchanger
import taxes.HTML._
import taxes.Market.Market

import scala.xml.Elem

trait Processed extends Boxed with ToHTML {
  val operationNumber : Int
}


object Processed {
  import HTML._

  case class Composed(operationNumber : Int, processed: Seq[Processed]) extends Processed {
    override def headerToHTML: Elem =
      processed.head.headerToHTML

    override def bodyToHTML: Elem =
      <span>{processed.map(_.bodyToHTML)}</span>
  }

  case class Exchange(
     operationNumber : Int
     , exchange : taxes.Exchange
     , baseCoinProxy : Market, baseCoinProxyRate : Double
     , boughtBasisPriceInBaseCoin : Double
     , soldPriceInBaseCoin : Double
     , proceedsInBaseCoin : Double
     , soldBasisInBaseCoin : Double
     , feeInBaseCoin : Double
     , gainInBaseCoin : Double
     , boughtSoldExchangeRate : Double, soldBoughtExchangeRate : Double
     , usedStocks : StockContainer
   ) extends Processed {

    override def headerToHTML: Elem =
      header4(
        <span class='operationNumber'>{operationNumber}</span>
        , df.format(exchange.date)
        , <span>
          Exchange of
          <span>
            {asMarket(exchange.fromAmount, exchange.fromMarket, decimals = 6)}
          </span>
          for
          <span>
            {asMarket(exchange.toAmount, exchange.toMarket, decimals = 6)}
          </span>
        </span>
        , <span class='exchanger'>{exchange.exchanger}</span>
      )

    override def bodyToHTML: Elem =
      <span>
        {if (exchange.description.nonEmpty)
        <div class='desc'>
          {exchange.description}
        </div>
        }
        <div class='rates'>
          {if(baseCoinProxy != baseMarket)
          <span>
            <span class='embold'>Proxy rate:</span>
            {asRate(baseCoinProxyRate, baseMarket, baseCoinProxy)}.
          </span>
          }
          <span class='embold'>Exchange rates:</span>
          {asRate(boughtSoldExchangeRate, exchange.toMarket, exchange.fromMarket)}
          ,
          {asRate(soldBoughtExchangeRate, exchange.fromMarket, exchange.toMarket)}
        </div>
        {if(exchange.toAmount > 0)
        <div class='marginBottom5'>
          <span class='embold'>Bought:</span>
          <span>
            {asMarket(exchange.toAmount, exchange.toMarket)}
            {if(exchange.toMarket != baseMarket)
            <span class='small2'>
              ({asRate(boughtBasisPriceInBaseCoin, baseMarket, exchange.toMarket)})
            </span>
            }
          </span>
        </div>
        }
        <div>
          <span class='embold'>Proceeds:</span>
          <span>
            {asMarket(exchange.fromAmount, exchange.fromMarket)}
          </span>
          {if(exchange.fromMarket != baseMarket) {
            <span class='small2'>
              ({asRate(soldPriceInBaseCoin, baseMarket, exchange.fromMarket)})
            </span>
            <span>=</span>
            <span>
              {asMarket(proceedsInBaseCoin, baseMarket)}
            </span>
           }
          }
        </div>
        {if(soldBasisInBaseCoin>0)
          <div>
            <span class='embold'>Cost basis:</span>
            <span>
              {asMarket(soldBasisInBaseCoin, baseMarket)}
            </span>
            {if(usedStocks.nonEmpty)
            <span class='stock'>. Used
              {(if(usedStocks.size > 1) "batches" else "batch")+":"}
              ({usedStocks.toHTML(showTotal = false)})
            </span>
            }
          </div>
        }
        {if(feeInBaseCoin > 0)
        <div>
          <span class='embold'>Fee:</span>
          <span>
            {asMarket(exchange.fee, exchange.feeMarket)}
          </span>{if(exchange.feeMarket != baseMarket)
          <span>
            =
            {asMarket(feeInBaseCoin, baseMarket)}
          </span>}
        </div>
        }
        {if(exchange.fromMarket != baseMarket)
        <span>
          <span class='embold'>
            {if(gainInBaseCoin > 0) "Gain:" else "Loss:"}
          </span>
          {asMarket(gainInBaseCoin.abs, baseMarket)}
        </span>}
      </span>
  }


  case class Gain(
     operationNumber : Int
     , gain : taxes.Gain
     , gainInBaseCoin : Double
     , basePrice : Double
   ) extends Processed {

    override def headerToHTML: Elem =
      header4(
        <span class='operationNumber'>{operationNumber}</span>
        , df.format(gain.date)
        , <span> Gain of
          {asMarket(gain.amount, gain.market, decimals = 6)}
        </span>
        , <span class='exchanger'>{gain.exchanger}</span>

      )

    override def bodyToHTML: Elem =
      <span>
        {if(gain.description.nonEmpty)
        <div class='desc'>
          {gain.description}
        </div>
        }
        <div>
          <span class='embold'>Gain:</span>
          {asMarket(gain.amount, gain.market)}
          {if(gain.market != baseMarket)
          <span class='small2'>
            ({asRate(basePrice, baseMarket, gain.market)})
          </span>
            <span>=</span>
          }
          {asMarket(gainInBaseCoin, baseMarket)}
        </div>
      </span>
  }


  case class Loss(
                   operationNumber : Int
                   , loss : taxes.Loss
                   , lossInBaseCoin : Double
                   , usedStocks : StockContainer
   ) extends Processed {

    override def headerToHTML: Elem =
      header4(
        <span class='operationNumber'>{operationNumber}</span>
        , df.format(loss.date)
        , <span> Loss of
          {asMarket(loss.amount, loss.market, decimals = 6)}
        </span>
        , <span class='exchanger'>{loss.exchanger}</span>
      )

    override def bodyToHTML: Elem =
      <span>
        {if(loss.description.nonEmpty)
        <div class='desc'>
          {loss.description}
        </div>
        }
        <div>
          <span class='embold'>Loss:</span>
          {asMarket(loss.amount, loss.market)}
          {if(loss.market != baseMarket)
          <span class='small2'>
            ({if(usedStocks.nonEmpty)
            <span>Used
              {(if(usedStocks.iterator.length > 1) "batches" else "batch")+":"}
              {usedStocks.toHTML(showTotal = false)}
            </span>
          else
            "no funds available"
            })
          </span>
            <span>=</span>
          }
          {asMarket(lossInBaseCoin, baseMarket)}
        </div>
      </span>
  }

  case class SettlementBuy(
    operationNumber : Int
    , settlement : taxes.SettlementBuy
  ) extends Processed {

    override def headerToHTML: Elem =
      header4(
        <span class='operationNumber'>{operationNumber}</span>
        , df.format(settlement.date)
        , <span> Settlement exchange of
          {asMarket(settlement.fromAmount, settlement.fromMarket, decimals = 6)}
          for
          {asMarket(settlement.toAmount, settlement.toMarket, decimals = 6)}
        </span>
        , <span class='exchanger'>{settlement.exchanger}</span>
      )

    override def bodyToHTML: Elem =
      <span>
        {if(settlement.description.nonEmpty)
        <div class='desc'>
          {settlement.description}
        </div>
        }
        {if(settlement.fee > 0)
        <div>
          <span class='embold'>Fee: </span>
          {asMarket(settlement.fee, settlement.feeMarket)}
        </div>
        }
      </span>
  }


  case class Fee(
        operationNumber : Int
        , fee : taxes.Fee
        , feeInBaseCoin : Double
        , usedStocks : StockContainer
  ) extends Processed {

    override def headerToHTML: Elem =
      header4(
        <span class='operationNumber'>{operationNumber}</span>
        , df.format(fee.date)
        , <span> Fee of
          {fee.alt match {
            case None => asMarket(fee.amount, fee.market, decimals = 6)
            case Some((amount, market)) => asMarket(amount, market, decimals = 6)
          }
          }
        </span>
        , <span class='exchanger'>{fee.exchanger}</span>
      )

    override def bodyToHTML: Elem =
      <span>
        {if(fee.description.nonEmpty)
        <div class='desc'>
          {fee.description}
        </div>
        }
        {if(true)
        <div>
          <span class='embold'>Fee:</span>
          {fee.alt match {
          case None => ;
          case Some((amount, market)) =>
            <span>{asMarket(amount, market)}=</span>
        }
          }
          {asMarket(fee.amount, fee.market)}
          {if(fee.market != baseMarket)
          <span class='small2'>
            ({if(usedStocks.nonEmpty)
            <span>Used
              {(if(usedStocks.iterator.length > 1) "batches" else "batch")+":"}
              {usedStocks.toHTML(showTotal = false)}
            </span>
          else
            "no funds available"
            })
          </span>
            <span>=</span>
          }
          {asMarket(feeInBaseCoin, baseMarket)}
        </div>
        }
      </span>
  }


  case class Margin(
     operationNumber : Int
     , date : Date
     , exchanger: Exchanger
     , what : String
     , fromAmount : Double, fromMarket : Market
     , toAmount : Double, toMarket : Market
     , exchangeRate : Double
     , description : String
     , usedStocksOpt : Option[StockContainer]
   ) extends Processed {

    private val isLong = what.toLowerCase.contains("long")
    private val isOpen = what.toLowerCase.contains("open")
    private val (amount2, market2, amount1, market1) =
      if(isLong == isOpen)
        (fromAmount, fromMarket, toAmount, toMarket)
      else
        (toAmount, toMarket, fromAmount, fromMarket)

    override def headerToHTML: Elem =
      header4(
        <span class='operationNumber'>{operationNumber}</span>
        , df.format(date)
        , <span> {what}
          {asMarket(amount1, market1, decimals = 6)}
          for
          {asMarket(amount2, market2, decimals = 6)}
        </span>
        , <span class='exchanger'>{exchanger}</span>
      )

    override def bodyToHTML: Elem =
      <span>
        {if(description.nonEmpty)
        <div class='desc'>
          {description}
        </div>
        }
        <div class='rates'>
          <span class='embold'>Exchange rates:</span>
          <span class='noLineBreak'>
            {asRate(exchangeRate, market2, market1)}
          </span>
        </div>
        {usedStocksOpt match {
          case Some(usedStocks) =>
            <div>
              <span class='embold'>Cost basis:</span>
              <span>
                {asMarket(usedStocks.totalCost, usedStocks.baseMarket)}
              </span>
              {if(usedStocksOpt.nonEmpty)
              <span class='small2'>. Used
                {(if(usedStocksOpt.iterator.length > 1) "batches" else "batch")+":"}
                ({usedStocks.toHTML(showTotal = false)})
              </span>
              }
            </div>
          case None => ;
          }
        }
      </span>
  }
}
