package taxes

import taxes.HTMLDoc._
import taxes.date._
import taxes.exchanger.Exchanger


trait Processed extends Boxed with ToHTML {
  val operationNumber : Int
}


object Processed {
  import HTMLDoc._

  private val headerDecimals = 6

  case class Composed(operationNumber : Int, processed: Seq[Processed]) extends Processed {
    override def headerToHTML: HTML =
      processed.head.headerToHTML

    override def bodyToHTML: HTML =
      <span>{processed.map(_.bodyToHTML)}</span>
  }

  case class Exchange( operationNumber : Int
                       , exchange : taxes.Exchange
                       , baseCoinProxy : Market, baseCoinProxyRate : Double
                       , boughtBasisPriceInBaseCoin : Double
                       , soldPriceInBaseCoin : Double
                       , proceedsInBaseCoin : Double
                       , soldBasisInBaseCoin : Double
                       , _deprecated_feeAmount : Double
                       , _deprecated_feeMarket : Market
                       , _deprecated_feeInBaseCoin : Double
                       , gainInBaseCoin : Double
                       , boughtSoldExchangeRate : Double, soldBoughtExchangeRate : Double
                       , usedStocks : StockContainer
                       , buys : StockContainer
                       , sells : StockContainer
                     ) extends Processed {

    override def headerToHTML: HTML =
      header4(
        <span class='operationNumber'>{operationNumber}</span>
        , exchange.date.format(df)
        , <span>
          {s"${if (exchange.isSettlement) "Settlement " else ""} Exchange of"}
          <span>
            {asMarket(exchange.fromAmount, exchange.fromMarket, decimals = headerDecimals)}
          </span>
          for
          <span>
            {asMarket(exchange.toAmount, exchange.toMarket, decimals = headerDecimals)}
          </span>
        </span>
        , <span class='exchanger'>{exchange.exchanger}</span>
      )

    override def bodyToHTML: HTML =
      <span>
        {if(Config.verbosity(Verbosity.showMoreDetails) && exchange.description.nonEmpty)
        <div class='desc'>
          {exchange.description}
        </div>
        }
        {if(Config.verbosity(Verbosity.showRates))
        <div class='rates'>
          {if(baseCoinProxy != baseMarket)
          <span>
            <span class='embold'>Proxy rate:</span>
            {asRate(baseCoinProxyRate, baseMarket, baseCoinProxy)}.
          </span>
          }
          {def isOK(x : Double) = !x.isNaN && !x.isInfinity
        if(isOK(boughtBasisPriceInBaseCoin) && isOK(soldBoughtExchangeRate))
          <span>
            <span class='embold'>Exchange rates:</span>
            {asRate(boughtSoldExchangeRate, exchange.toMarket, exchange.fromMarket)}
            ,
            {asRate(soldBoughtExchangeRate, exchange.fromMarket, exchange.toMarket)}
          </span>
          }
        </div>
        }
        {if(Config.verbosity(Verbosity.showDetails) && exchange.toAmount > 0)
        <div class='marginBottom5'>
          <span class='embold'>Bought:</span>
          <span>
            {asMarket(exchange.toAmount, exchange.toMarket)}
            {if(Config.verbosity(Verbosity.showRates) && exchange.toMarket != baseMarket)
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
          {if(exchange.fromMarket != baseMarket)
          <span>
            {if(Config.verbosity(Verbosity.showRates))
            <span class='small2'>
              ({asRate(soldPriceInBaseCoin, baseMarket, exchange.fromMarket)})
            </span>
            }
            =
            {asMarket(proceedsInBaseCoin, baseMarket)}
          </span>
          }
        </div>
        {if(soldBasisInBaseCoin>0)
        <div>
          <span class='embold'>Cost basis:</span>
          <span>
            {asMarket(soldBasisInBaseCoin, baseMarket)}
          </span>
          {if(Config.verbosity(Verbosity.showRates) && usedStocks.nonEmpty && exchange.fromMarket != baseMarket)
          <span class='stock'>. Used
            {s"${if (usedStocks.size > 1) "batches" else "batch"}:"}
            ({usedStocks.toHTML(showTotal = Config.verbosity(Verbosity.showAll))})
          </span>
          }
        </div>
        }
        {if(_deprecated_feeInBaseCoin > 0)
        <div>
          <span class='embold'>Fee:</span>
          <span>
            {asMarket(_deprecated_feeAmount, _deprecated_feeMarket)}
          </span>
          {if(_deprecated_feeMarket != baseMarket)
          <span>
            =
            {asMarket(_deprecated_feeInBaseCoin, baseMarket)}
          </span>
          }
        </div>
        }
        {if(exchange.fromMarket != baseMarket)
        <span>
          <span class='embold'>
            {if(gainInBaseCoin > 0) "Gain:" else "Loss:"}
          </span>
          {asMarket(gainInBaseCoin.abs, baseMarket)}
        </span>
        }
        {if(Config.verbosity(Verbosity.showStocks))
        <div class='stock marginTopBottom20'>
          <div class='embold'>Bought:</div>
          <div>{buys.toHTML(showTotal = Config.verbosity(Verbosity.showAll))}</div>
          <div class='embold'>Sold:</div>
          <div>{sells.toHTML(showTotal = Config.verbosity(Verbosity.showAll))}</div>
        </div>
        }
      </span>
  }


  case class Gain(operationNumber : Int
                   , gain : taxes.Gain
                   , gainInBaseCoin : Double
                   , basePrice : Double
                   , stocks : StockContainer
                 ) extends Processed {

    override def headerToHTML: HTML =
      header4(
        <span class='operationNumber'>{operationNumber}</span>
        , gain.date.format(df)
        , <span> Gain of
          {asMarket(gain.amount, gain.market, decimals = headerDecimals)}
        </span>
        , <span class='exchanger'>{gain.exchanger}</span>

      )

    override def bodyToHTML: HTML =
      <span>
        {if(Config.verbosity(Verbosity.showMoreDetails) && gain.description.nonEmpty)
        <div class='desc'>
          {gain.description}
        </div>
        }
        <div>
          <span class='embold'>Gain:</span>
          {asMarket(gain.amount, gain.market)}
          {if(gain.market != baseMarket)
          <span>
            {if(Config.verbosity(Verbosity.showRates))
            <span class='small2'>
              ({asRate(basePrice, baseMarket, gain.market)})
            </span>
            }
            = {asMarket(gainInBaseCoin, baseMarket)}
          </span>
          }
        </div>
        {if(Config.verbosity(Verbosity.showStocks))
        <div class='stock marginTopBottom20'>
          <div class='embold'>Stocks:</div>
          <div>{stocks.toHTML(showTotal = Config.verbosity(Verbosity.showAll))}</div>
        </div>
        }
      </span>
  }


  case class Loss(operationNumber : Int
                   , loss : taxes.Loss
                   , lossInBaseCoin : Double
                   , usedStocks : StockContainer
                   , stocks : StockContainer
                 ) extends Processed {

    override def headerToHTML: HTML =
      header4(
        <span class='operationNumber'>{operationNumber}</span>
        , loss.date.format(df)
        , <span> Loss of
          {asMarket(loss.amount, loss.market, decimals = headerDecimals)}
        </span>
        , <span class='exchanger'>{loss.exchanger}</span>
      )

    override def bodyToHTML: HTML =
      <span>
        {if(Config.verbosity(Verbosity.showMoreDetails) && loss.description.nonEmpty)
        <div class='desc'>
          {loss.description}
        </div>
        }
        <div>
          <span class='embold'>Loss:</span>
          {asMarket(loss.amount, loss.market)}
          {if(loss.market != baseMarket)
          <span>
            {if(Config.verbosity(Verbosity.showRates))
            <span class='small2'>
              ({if(usedStocks.nonEmpty)
              <span>Used
                {s"${if (usedStocks.iterator.length > 1) "batches" else "batch"}:"}
                {usedStocks.toHTML(showTotal = Config.verbosity(Verbosity.showAll))}
              </span>
            else
              "no funds available"
              })
            </span>
            }
            = {asMarket(lossInBaseCoin, baseMarket)}
          </span>
          }
        </div>
        {if(Config.verbosity(Verbosity.showStocks))
        <div class='stock marginTopBottom20'>
          <div class='embold'>Stocks:</div>
          <div>{stocks.toHTML(showTotal = Config.verbosity(Verbosity.showAll))}</div>
        </div>
        }
      </span>
  }

  case class Fee(operationNumber : Int
                  , fee : taxes.Fee
                  , feeInBaseCoin : Double
                  , usedStocks : StockContainer
                  , stocks : StockContainer
                ) extends Processed {

    override def headerToHTML: HTML =
      header4(
        <span class='operationNumber'>{operationNumber}</span>
        , fee.date.format(df)
        , <span> Fee of
          {fee.alt match {
            case None => asMarket(fee.amount, fee.market, decimals = headerDecimals)
            case Some((amount, market)) => asMarket(amount, market, decimals = headerDecimals)
          }
          }
        </span>
        , <span class='exchanger'>{fee.exchanger}</span>
      )

    override def bodyToHTML: HTML =
      <span>
        {if(Config.verbosity(Verbosity.showMoreDetails) && fee.description.nonEmpty)
        <div class='desc'>
          {fee.description}
        </div>
        }
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
          <span>
            {if(fee.market != baseMarket && Config.verbosity(Verbosity.showRates))
            <span class='small2'>
              ({if(usedStocks.nonEmpty)
              <span>
                Used {s"${if (usedStocks.iterator.length > 1) "batches" else "batch"}:"}
                {usedStocks.toHTML(showTotal = Config.verbosity(Verbosity.showAll))}
              </span>
            else
              "no funds available"
              })
            </span>
            }
            = {asMarket(feeInBaseCoin, baseMarket)}
          </span>
          }
        </div>
        {if(Config.verbosity(Verbosity.showStocks))
        <div class='stock marginTopBottom20'>
          <div class='embold'>Stocks:</div>
          <div>{stocks.toHTML(showTotal = Config.verbosity(Verbosity.showAll))}</div>
        </div>
        }
      </span>
  }


  case class Margin(operationNumber : Int
                     , date : LocalDateTime
                     , exchanger: Exchanger
                     , what : String
                     , fromAmount : Double, fromMarket : Market
                     , toAmount : Double, toMarket : Market
                     , exchangeRate : Double
                     , description : String
                     , usedStocksOpt : Option[StockContainer]
                     , marginBuys : StockContainer
                     , marginSells : StockContainer
                   ) extends Processed {

    private val isLong = what.toLowerCase.contains("long")
    private val isOpen = what.toLowerCase.contains("open")
    private val (amount2, market2, amount1, market1) =
      if(isLong == isOpen)
        (fromAmount, fromMarket, toAmount, toMarket)
      else
        (toAmount, toMarket, fromAmount, fromMarket)

    override def headerToHTML: HTML =
      header4(
        <span class='operationNumber'>{operationNumber}</span>
        , date.format(df)
        , <span> {what}
          {asMarket(amount1, market1, decimals = headerDecimals)}
          for
          {asMarket(amount2, market2, decimals = headerDecimals)}
        </span>
        , <span class='exchanger'>{exchanger}</span>
      )

    override def bodyToHTML: HTML =
      <span>
        {if(Config.verbosity(Verbosity.showMoreDetails) && description.nonEmpty)
        <div class='desc'>
          {description}
        </div>
        }
        {if(Config.verbosity(Verbosity.showRates))
        <div class='rates'>
          <span class='embold'>Exchange rates:</span>
          <span class='noLineBreak'>
            {asRate(exchangeRate, market2, market1)}
          </span>
        </div>
        }
        {usedStocksOpt match {
        case Some(usedStocks) =>
          <div>
            <span class='embold'>Cost basis:</span>
            <span>{asMarket(usedStocks.totalCost, usedStocks.baseMarket)}</span>
            {if(Config.verbosity(Verbosity.showRates) && usedStocksOpt.nonEmpty)
            <span class='small2'>. Used
              {s"${if (usedStocksOpt.iterator.length > 1) "batches" else "batch"}:"}
              ({usedStocks.toHTML(showTotal = Config.verbosity(Verbosity.showAll))})
            </span>
            }
          </div>
        case None => ;
      }
        }
        {if(Config.verbosity(Verbosity.showStocks))
        <div class='stock marginTopBottom20'>
          <div class='embold'>Margin buys:</div>
          <div>{marginBuys.toHTML(showTotal = Config.verbosity(Verbosity.showAll))}</div>
          <div class='embold'>Margin sells:</div>
          <div>{marginSells.toHTML(showTotal = Config.verbosity(Verbosity.showAll))}</div>
        </div>
        }
      </span>
  }
}
