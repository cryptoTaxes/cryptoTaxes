package taxes

import java.io.{File, PrintStream}
import taxes.Market.Market
import taxes.Util.Logger
import taxes.Util.Parse.{CSVReader, Scanner, SeparatedScanner}

object TransactionsCache extends Initializable with Finalizable {

  case class TxKey(market: Market, txid : String, address : String)
  case class TxInfo(amount : Double, fee : Double, date : Date)

  private val map = scala.collection.mutable.Map[TxKey, TxInfo]()

  private val fileName = Paths.cacheFolder+"/"+"transactions.csv"

  private val df = "yyyy-MM-dd hh:mm:ss"
  private val sdf = new java.text.SimpleDateFormat(df)

  def saveToDisk(): Unit = {
    val f = new File(fileName)
    val ps = new PrintStream(f)
    val header = "market,txid,address,amount,fee,date"
    ps.println(header)
    for((key,info) <- map)
      ps.println(List(key.market, key.txid, key.address, info.amount, info.fee, sdf.format(info.date)).mkString(","))
    ps.close()
  }

  def loadFromDisk(): Unit = {
    def cacheReader(fileName : String) = new CSVReader[(TxKey, TxInfo)](fileName) {
      override val hasHeader: Boolean = true

      override def lineScanner(line: String) =
        SeparatedScanner(line, "[,]")

      override def readLine(line: String, scLn: Scanner) : CSVReader.Result[(TxKey, TxInfo)] = {
        val market = scLn.next()
        val txid = scLn.next()
        val address = scLn.next()
        val amount = scLn.nextDouble()
        val fee = scLn.nextDouble()
        val date = Date.fromString(scLn.next(), df)
        return CSVReader.Ok((TxKey(market, txid, address), TxInfo(amount, fee, date)))
      }
    }
    if(new File(fileName).exists())
      for((key, info) <- cacheReader(fileName).read())
        map += (key -> info)
  }

  def lookup(market: Market, txid : String, address : String) : TxInfo = {
    map.get(TxKey(market, txid, address)) match {
      case Some(txInfo) => txInfo
      case None => {
        if (List(Market.bitcoin, Market.litecoin, Market.dogecoin, Market.etc, Market.vertcoin).contains(market)) {
          val sc = BlockExplorerSearcher(market, txid, address)
          val key = TxKey(market, txid, address)
          val info = TxInfo(sc.amount, sc.fee, sc.date)
          map += (key -> info)
          saveToDisk()
          return info
        } else {
          Logger.fatal("TransactionCache not implemented yet for %s. %s.".format(market, txid))
        }
      }
    }
  }

  override def init(): Unit = {
    loadFromDisk()
  }

  override def wrapUp(): Unit = {
    saveToDisk()
  }
}
