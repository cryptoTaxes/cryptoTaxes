package taxes

import taxes.HTMLDoc._
import taxes.date._
import taxes.exchanger.Exchanger
import spray.json._
import spray.json.JsonProtocol._
import taxes.report.Format.{asCurrency, asRate}

trait Processed extends Boxed with ToHTML {
  val operationNumber: Int
}


object Processed {
  import HTMLDoc._

  private lazy val baseCurrency = Config.config.baseCurrency.currency

  private val df = Format.shortDf

  private val headerDecimals = 4

  case class Composed(operationNumber: Int, processed: Seq[Processed]) extends Processed {
    override def headerToHTML: HTML =
      processed.head.headerToHTML

    override def bodyToHTML: HTML =
      <span>{processed.map(_.bodyToHTML)}</span>
  }


  def operationNumberAnchor(operationNumber: Int): HTML =
    <span class='opNumber' id={s"$operationNumber"}>{operationNumber}</span>

  case class Exchange(operationNumber: Int
                      , exchange: taxes.Exchange
                      , baseCurrencyProxy: Currency, baseCurrencyProxyRate: Price
                      , boughtBasisPriceInBaseCurrency: Price
                      , soldPriceInBaseCurrency: Price
                      , proceedsInBaseCurrency: Double
                      , soldBasisInBaseCurrency: Double
                      , _deprecated_feeAmount: Double
                      , _deprecated_feeCurrency: Currency
                      , _deprecated_feeInBaseCurrency: Double
                      , gainInBaseCurrency: Double
                      , boughtSoldExchangeRate: Price, soldBoughtExchangeRate: Price
                      , disposedStocks: StockContainer
                      , toStocks: StockContainer // current state of involved stocks
                      , fromStocks: StockContainer
                     ) extends Processed {

    override def headerToHTML: HTML =
      header4(
        operationNumberAnchor(operationNumber)
        , exchange.date.format(df)
        , <span>
          {s"${if(exchange.isSettlement) "Settlement " else ""} Exchange of"}
          <span>
            {asCurrency(exchange.fromAmount, exchange.fromCurrency, decimals = headerDecimals, small = false)}
          </span>
          for
          <span>
            {asCurrency(exchange.toAmount, exchange.toCurrency, decimals = headerDecimals, small = false)}
          </span>
        </span>
        , <span class='exchanger'>{exchange.exchanger}</span>
      )

    override def bodyToHTML: HTML =
      <span>
        {if(Config.verbosity(Verbosity.showMoreDetails) && exchange.description.nonEmpty)
        <div class='desc'>
          {exchange.description.toHTML}
        </div>
        }
        {if(Config.verbosity(Verbosity.showRates))
        <div class='rates'>
          {if(baseCurrencyProxy != baseCurrency)
          <span>
            <span class='embold'>Proxy rate:</span>
            {asRate(baseCurrencyProxyRate, baseCurrency, baseCurrencyProxy)}.
          </span>
          }
          {def isOK(x: Double) = !x.isNaN && !x.isInfinity
        if(isOK(boughtBasisPriceInBaseCurrency) && isOK(soldBoughtExchangeRate))
          <span>
            <span class='embold'>Exchange rates:</span>
            {asRate(boughtSoldExchangeRate, exchange.toCurrency, exchange.fromCurrency)}
            ,
            {asRate(soldBoughtExchangeRate, exchange.fromCurrency, exchange.toCurrency)}
          </span>
          }
        </div>
        }
        {if(Config.verbosity(Verbosity.showDetails) && exchange.toAmount > 0)
        <div class='marginBottom5'>
          <span class='embold'>Bought:</span>
          <span>
            {asCurrency(exchange.toAmount, exchange.toCurrency)}
            {if(Config.verbosity(Verbosity.showRates) && exchange.toCurrency != baseCurrency)
            <span class='small2'>
              ({asRate(boughtBasisPriceInBaseCurrency, baseCurrency, exchange.toCurrency)})
            </span>
            }
          </span>
        </div>
        }
        <div>
          <span class='embold'>Proceeds:</span>
          <span>
            {asCurrency(exchange.fromAmount, exchange.fromCurrency)}
          </span>
          {if(exchange.fromCurrency != baseCurrency)
          <span>
            {if(Config.verbosity(Verbosity.showRates))
            <span class='small2'>
              ({asRate(soldPriceInBaseCurrency, baseCurrency, exchange.fromCurrency)})
            </span>
            }
            =
            {asCurrency(proceedsInBaseCurrency, baseCurrency)}
          </span>
          }
        </div>
        {if(soldBasisInBaseCurrency>0)
        <div>
          <span class='embold'>Cost basis:</span>
          <span>
            {asCurrency(soldBasisInBaseCurrency, baseCurrency)}
          </span>
          {if(Config.verbosity(Verbosity.showRates) && disposedStocks.nonEmpty && exchange.fromCurrency != baseCurrency)
          <span class='stock'>. Used
            {s"${if(disposedStocks.size > 1) "batches" else "batch"}:"}
            ({disposedStocks.toHTML(showTotal = Config.verbosity(Verbosity.showAll))})
          </span>
          }
        </div>
        }
        {if(_deprecated_feeInBaseCurrency > 0)
        <div>
          <span class='embold'>Fee:</span>
          <span>
            {asCurrency(_deprecated_feeAmount, _deprecated_feeCurrency)}
          </span>
          {if(_deprecated_feeCurrency != baseCurrency)
          <span>
            =
            {asCurrency(_deprecated_feeInBaseCurrency, baseCurrency)}
          </span>
          }
        </div>
        }
        {if(exchange.fromCurrency != baseCurrency)
        <span>
          <span class='embold'>
            {if(gainInBaseCurrency > 0) "Gain:" else "Loss:"}
          </span>
          {asCurrency(gainInBaseCurrency.abs, baseCurrency)}
        </span>
        }
        {if(Config.verbosity(Verbosity.showStocks))
        <div class='stock marginTopBottom20'>
          <div class='embold'>Bought:</div>
          <div>{toStocks.toHTML(showTotal = Config.verbosity(Verbosity.showAll))}</div>
          <div class='embold'>Sold:</div>
          <div>{fromStocks.toHTML(showTotal = Config.verbosity(Verbosity.showAll))}</div>
        </div>
        }
      </span>
  }


  case class Gain( operationNumber: Int
                 , gain: taxes.Gain
                 , gainInBaseCurrency: Double
                 , basePrice: Price
                 , stocks: StockContainer
                 ) extends Processed {

    override def headerToHTML: HTML =
      header4(
        operationNumberAnchor(operationNumber)
        , gain.date.format(df)
        , <span> Gain of
          {asCurrency(gain.amount, gain.currency, decimals = headerDecimals, small = false)}
        </span>
        , <span class='exchanger'>{gain.exchanger}</span>
      )

    override def bodyToHTML: HTML =
      <span>
        {if(Config.verbosity(Verbosity.showMoreDetails) && gain.description.nonEmpty)
        <div class='desc'>
          {gain.description.toHTML}
        </div>
        }
        <div>
          <span class='embold'>Gain:</span>
          {asCurrency(gain.amount, gain.currency)}
          {if(gain.currency != baseCurrency)
          <span>
            {if(Config.verbosity(Verbosity.showRates))
            <span class='small2'>
              ({asRate(basePrice, baseCurrency, gain.currency)})
            </span>
            }
            = {asCurrency(gainInBaseCurrency, baseCurrency)}
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


  case class Loss(operationNumber: Int
                  , loss: taxes.Loss
                  , lossInBaseCurrency: Double
                  , disposedStocks: StockContainer
                  , stocks: StockContainer
                 ) extends Processed {

    override def headerToHTML: HTML =
      header4(
        operationNumberAnchor(operationNumber)
        , loss.date.format(df)
        , <span> Loss of
          {asCurrency(loss.amount, loss.currency, decimals = headerDecimals, small = false)}
        </span>
        , <span class='exchanger'>{loss.exchanger}</span>
      )

    override def bodyToHTML: HTML =
      <span>
        {if(Config.verbosity(Verbosity.showMoreDetails) && loss.description.nonEmpty)
        <div class='desc'>
          {loss.description.toHTML}
        </div>
        }
        <div>
          <span class='embold'>Loss:</span>
          {asCurrency(loss.amount, loss.currency)}
          {if(loss.currency != baseCurrency)
          <span>
            {if(Config.verbosity(Verbosity.showRates))
            <span class='small2'>
              {if(disposedStocks.nonEmpty)
              <span>(Used
                {s"${if(disposedStocks.iterator.length > 1) "batches" else "batch"}:"}
                {disposedStocks.toHTML(showTotal = Config.verbosity(Verbosity.showAll))})
              </span>
            else
              ""
              }
            </span>
            }
            = {asCurrency(lossInBaseCurrency, baseCurrency)}
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


  case class Fee(operationNumber: Int
                 , fee: taxes.Fee
                 , feeInBaseCurrency: Double
                 , disposedStocks: StockContainer
                 , stocks: StockContainer
                ) extends Processed {

    override def headerToHTML: HTML =
      header4(
        operationNumberAnchor(operationNumber)
        , fee.date.format(df)
        , <span> Fee of
          {fee.alt match {
            case None => asCurrency(fee.amount, fee.currency, decimals = headerDecimals, small = false)
            case Some((amount, currency)) => asCurrency(amount, currency, decimals = headerDecimals, small = false)
          }
          }
        </span>
        , <span class='exchanger'>{fee.exchanger}</span>
      )

    override def bodyToHTML: HTML =
      <span>
        {if(Config.verbosity(Verbosity.showMoreDetails) && fee.description.nonEmpty)
        <div class='desc'>
          {fee.description.toHTML}
        </div>
        }
        <div>
          <span class='embold'>Fee:</span>
          {fee.alt match {
          case None => ;
          case Some((amount, currency)) =>
            <span>{asCurrency(amount, currency)}=</span>
        }
          }
          {asCurrency(fee.amount, fee.currency)}
          {if(fee.currency != baseCurrency)
          <span>
            {if(fee.currency != baseCurrency && Config.verbosity(Verbosity.showRates))
            <span class='small2'>
              {if(disposedStocks.nonEmpty)
              <span>
                (Used {s"${if(disposedStocks.iterator.length > 1) "batches" else "batch"}:"}
                {disposedStocks.toHTML(showTotal = Config.verbosity(Verbosity.showAll))})
              </span>
            else
              ""
              }
            </span>
            }
            = {asCurrency(feeInBaseCurrency, baseCurrency)}
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


  case class Margin(operationNumber: Int
                    , date: LocalDateTime
                    , exchanger: Exchanger
                    , what: String
                    , fromAmount: Double, fromCurrency: Currency
                    , toAmount: Double, toCurrency: Currency
                    , exchangeRate: Price
                    , description: RichText
                    , disposedStocksOpt: Option[StockContainer]
                    , longsStocks: StockContainer
                    , shortsStocks: StockContainer
                   ) extends Processed {

    private val isLong = what.toLowerCase.contains("long")
    private val isOpen = what.toLowerCase.contains("open")
    private val (amount2, currency2, amount1, currency1) =
      if(isLong == isOpen)
        (fromAmount, fromCurrency, toAmount, toCurrency)
      else
        (toAmount, toCurrency, fromAmount, fromCurrency)

    override def headerToHTML: HTML =
      header4(
        operationNumberAnchor(operationNumber)
        , date.format(df)
        , <span> {what}
          {asCurrency(amount1, currency1, decimals = headerDecimals, small = false)}
          for
          {asCurrency(amount2, currency2, decimals = headerDecimals, small = false)}
        </span>
        , <span class='exchanger'>{exchanger}</span>
      )

    override def bodyToHTML: HTML =
      <span>
        {if(Config.verbosity(Verbosity.showMoreDetails) && description.nonEmpty)
        <div class='desc'>
          {description.toHTML}
        </div>
        }
        {if(Config.verbosity(Verbosity.showRates))
        <div class='rates'>
          <span class='embold'>Exchange rates:</span>
          <span class='noLineBreak'>
            {asRate(exchangeRate, currency2, currency1)}
          </span>
        </div>
        }
        {disposedStocksOpt match {
        case Some(usedStocks) =>
          <div>
            <span class='embold'>Cost basis:</span>
            <span>{asCurrency(usedStocks.totalCost, usedStocks.baseCurrency)}</span>
            {if(Config.verbosity(Verbosity.showRates) && disposedStocksOpt.nonEmpty)
            <span class='small2'>. Used
              {s"${if(disposedStocksOpt.iterator.length > 1) "batches" else "batch"}:"}
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
          <div>{longsStocks.toHTML(showTotal = Config.verbosity(Verbosity.showAll))}</div>
          <div class='embold'>Margin sells:</div>
          <div>{shortsStocks.toHTML(showTotal = Config.verbosity(Verbosity.showAll))}</div>
        </div>
        }
      </span>
  }



  implicit val exchangeJson = jsonFormat17(Exchange)
  implicit val gainJson = jsonFormat5(Gain)
  implicit val lossJson = jsonFormat5(Loss)
  implicit val feeJson = jsonFormat5(Fee)
  //implicit val marginJson: JsonFormat[Margin] = rootFormat((jsonFormat13(Margin)))
  implicit val marginJson = jsonFormat(Margin,"operationNumber", "date", "exchanger", "what", "fromAmount", "fromCurrency", "toAmount", "toCurrency","exchangeRate","description", "usedStocksOpt", "marginBuys", "marginSells")
  implicit val processedJson = new RootJsonFormat[Processed] {
    val _Composed = "Composed"
    val _Exchange = "Exchange"
    val _Margin = "Margin"
    val _Fee = "Fee"
    val _Loss = "Loss"
    val _Gain = "Gain"

    val _type = "type"
    val _operation = "operation"

    def write(processed: Processed) = {
      val (tag, json) = processed match {
        case c: Composed => (_Composed, composedJson.write(c))
        case e: Exchange => (_Exchange, exchangeJson.write(e))
        case m: Margin => (_Margin, marginJson.write(m))
        case f: Fee => (_Fee, feeJson.write(f))
        case l: Loss => (_Loss, lossJson.write(l))
        case g: Gain => (_Gain, gainJson.write(g))
      }
      JsObject(_type -> JsString(tag), _operation -> json)
    }

    def read(value: JsValue): Processed =
      try {
        value.asJsObject.getFields(_type, _operation) match {
          case Seq(JsString(tag), jsObj) =>
            tag match {
              case `_Composed` => composedJson.read(jsObj)
              case `_Exchange` => exchangeJson.read(jsObj)
              case `_Margin` => marginJson.read(jsObj)
              case `_Fee` => feeJson.read(jsObj)
              case `_Loss` => lossJson.read(jsObj)
              case `_Gain` => gainJson.read(jsObj)
            }
        }
      } catch {
        case _ => deserializationError(s"Processed expected in $value")
      }
  }
  implicit val composedJson: JsonFormat[Composed] = jsonFormat2(Composed)
}
