package taxes

import java.io.{File, PrintStream}

import taxes.Market.Market
import taxes.Util.Logger
import taxes.Util.Parse.{CSVReader, Scanner, SeparatedScanner}

import scala.io.Source

object TransactionsCache extends Initializable with Finalizable {

  case class TxKey(market: Market, txid : String, address : String)
  case class TxInfo(amount : Double, fee : Double)

  private val map = scala.collection.mutable.Map[TxKey, TxInfo]()

  private val fileName = Paths.cacheFolder+"/"+"transactions.csv"

  def saveToDisk(): Unit = {
    val f = new File(fileName)
    val ps = new PrintStream(f)
    val header = "market,txid,address,amount,fee"
    ps.println(header)
    for((key,info) <- map)
      ps.println(List(key.market, key.txid, key.address, info.amount, info.fee).mkString(","))
    ps.close()
  }

  def loadFromDisk(): Unit = {
    def cacheReader(fileName : String) = new CSVReader[(TxKey, TxInfo)](fileName) {
      override val hasHeader: Boolean = true

      override def lineScanner(line: String) =
        SeparatedScanner(line, "[,]")

      override def readLine(line: String, scLn: Scanner) : Either[String,(TxKey, TxInfo)] = {
        val market = scLn.next()
        val txid = scLn.next()
        val address = scLn.next()
        val amount = scLn.nextDouble()
        val fee = scLn.nextDouble()
        return Right((TxKey(market, txid, address), TxInfo(amount, fee)))
      }
    }
    for((key, info) <- cacheReader(fileName).read())
      map += (key -> info)
  }

  def lookup(market: Market, txid : String, address : String) : TxInfo = {
    map.get(TxKey(market, txid, address)) match {
      case Some(txInfo) => txInfo
      case None => {
        if(market == Market.bitcoin) {
          //toDo fix HTTP response code: 429
          val satoshi = 1E8
          val amount = Source.fromURL("https://blockchain.info/q/txresult/%s/%s".format(txid, address)).mkString.toInt / satoshi
          val fee = Source.fromURL("https://blockchain.info/q/txfee/%s".format(txid)).mkString.toInt / satoshi
          val key = TxKey(market, txid, address)
          val info = TxInfo(amount, fee)
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
