package taxes

import taxes.Market.Market
import taxes.Util.Logger
import taxes.Util.Parse.Parse

object BlockExplorerSearcher {
  def apply(market : Market, txid : String, address : String) =
    new BlockExplorerSearcher(market, txid, address)

  def fromURL(urlString : String) : String = {
    import java.io.BufferedInputStream
    import java.net.{URL, HttpURLConnection}

    val sb = new StringBuilder
    var conn : HttpURLConnection = null
    var in : BufferedInputStream = null

    try {
      val url = new URL(urlString)
      conn = url.openConnection.asInstanceOf[HttpURLConnection]
      conn.addRequestProperty("User-Agent", "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.11 (KHTML, like Gecko) Chrome/23.0.1271.95 Safari/537.11")
      conn.connect()
      in = new BufferedInputStream(conn.getInputStream())
      var ch = in.read()
      while(ch != -1) {
        sb.append(ch.toChar)
        ch = in.read()
      }
    } finally {
      if(in != null)
        in.close()
      if(conn != null)
        conn.disconnect()
    }
    return sb.toString()
  }

  private def locateAndSkip(str : String, prefix : String, toSkip : Char, numSkip : Int, endToken : String) : String = {
    val before = str.indexOf(prefix)
    var str1 = str.drop(before +  prefix.length)
    var found = 0
    while(found < numSkip) {
      if(str1.head == toSkip)
        found += 1
      str1 = str1.tail
    }

    var token = ""
    while(!str1.startsWith(endToken)) {
      val h = str1.head
      if(h != '\n' && h != '\r')
        token += h
      str1 = str1.tail
    }

    return token
  }

  def btcScrap(txid : String, address : String) : (Date, Double, Double) = {
    val url = "https://blockchain.info/tx/%s".format(txid)
    val str = BlockExplorerSearcher.fromURL(url)

    val dateStr = locateAndSkip(str, "<td>Received Time</td>", '>', 1, "</td>")
    val date = Date.fromString(dateStr, "yyyy-MM-dd hh:mm:ss")

    val amountStr = locateAndSkip(str, "%s</a>".format(address), '>', 2, " BTC")
    val amount = Parse.asDouble(amountStr)

    val feeStr = locateAndSkip(str, "<td>Fees</td>", '>', 2, " BTC")
    val fee = Parse.asDouble(feeStr)

    return (date, amount, fee)
  }

  def etcScrap(txid : String, address : String) : (Date, Double, Double) = {
    val url = "https://gastracker.io/tx/%s".format(txid)
    val str = BlockExplorerSearcher.fromURL(url)

    val dateStr = locateAndSkip(str, "<dt>Timestamp</dt>", '>', 1, "</dd>")
    val date = Date.fromString(dateStr, "EE MMM dd hh:mm:ss zzz yyyy")

    val amountStr = locateAndSkip(str, "<dt>Value</dt>", '>', 1, " Ether")
    val amount = Parse.asDouble(amountStr)

    val gasUsedStr = locateAndSkip(str, "<dt>Gas Used</dt>", '>', 1, "</dd>")
    val gasUsed = Parse.asInt(gasUsedStr)

    val gasPriceStr = locateAndSkip(str, "<dt>Gas Price</dt>", '>', 1, " Mwei")
    val gasPrice = Parse.asInt(gasPriceStr)

    val addressStr = locateAndSkip(str, "<dt>To</dt>", '>', 2, "</a>")
    if(address.toUpperCase != addressStr.toUpperCase())
      Logger.fatal("BlockExplorerScraper.etcScrap: address doesn't match %s %s".format(address, addressStr))

    val fee = gasPrice * gasUsed * 1E-12

    return (date, amount, fee)
  }

  def chainzCryptoidInfoScrap(coin : String, txid : String, address : String) : (Date, Double, Double) = {
    import scala.util.parsing.json.JSON
    import scala.collection.immutable.Map

    val url = "https://chainz.cryptoid.info/%s/api.dws?q=txinfo;t=%s".format(coin,txid)
    val response = BlockExplorerSearcher.fromURL(url)

    val jsonMap = JSON.parseFull(response)

    jsonMap match {
      case None => Logger.fatal("BlockExplorerScraper.chainzCryptoidInfoScrap: could not parse JSON %s".format(response))
      case Some(m) => {
        val map = m.asInstanceOf[Map[String, Any]]
        val date = Date.fromUnix(map("timestamp").asInstanceOf[Double].toLong)
        val fee = map("fees").asInstanceOf[Double]
        val outputs = map.asInstanceOf[Map[String, List[Map[String, Any]]]]("outputs")

        var found = false
        val it = outputs.iterator
        var amount = 0.0
        while (!found && it.hasNext) {
          val m = it.next()
          val addr = m("addr").asInstanceOf[String]
          if (addr == address) {
            amount = m("amount").asInstanceOf[Double]
            found = true
          }
        }

        if (!found)
          Logger.fatal("BlockExplorerScraper.chainzCryptoidInfoScrap: output address %s not found in JSON %s".format(address, response))

        return (date, amount, fee)
      }
    }
  }

  def ltcScrap(txid : String, address : String) : (Date, Double, Double) =
    chainzCryptoidInfoScrap("ltc", txid, address)

  def vtcScrap(txid : String, address : String) : (Date, Double, Double) =
    chainzCryptoidInfoScrap("vtc", txid, address)

  def dogeScrap(txid : String, address : String) : (Date, Double, Double) = {
    import scala.util.parsing.json.JSON
    import scala.collection.immutable.Map

    val url = "https://dogechain.info/api/v1/transaction/%s".format(txid)
    val response = BlockExplorerSearcher.fromURL(url)

    val jsonMap = JSON.parseFull(response)

    jsonMap match {
      case None => Logger.fatal("BlockExplorerScraper.dogeScrap: could not parse JSON %s".format(response))
      case Some(m) => {
        val map0 = m.asInstanceOf[Map[String, Any]]

        if(map0("success").asInstanceOf[Double] != 1)
          Logger.fatal("BlockExplorerScraper.dogeScrap: could not find transaction %s".format(txid))

        val map1 = map0("transaction").asInstanceOf[Map[String,Any]]

        val date = Date.fromUnix(map1("time").asInstanceOf[Double].toLong)

        val outputs = map1.asInstanceOf[Map[String, List[Map[String, Any]]]]("outputs")

        val Shibetoshi = 1E8

        var amount = 0.0
        var found = false

        var sumOuts = 0L
        for(m <- outputs) {
          val addr = m("address").asInstanceOf[String]
          val am = Parse.asDouble(m("value").asInstanceOf[String])
          sumOuts += (am * Shibetoshi).toLong

          if(addr == address) {
            amount = am
            found = true
          }
        }

        if(!found)
          Logger.fatal("BlockExplorerScraper.dogeScrap: output address %s not found in JSON %s".format(address, response))

        val inputs = map1.asInstanceOf[Map[String,List[Map[String, Any]]]]("inputs")

        var sumIns = 0L
        for(m <- inputs) {
          val addr = m("address").asInstanceOf[String]
          val am = Parse.asDouble(m("value").asInstanceOf[String])
          sumIns += (am * Shibetoshi).toLong
        }

        val fee = (sumIns - sumOuts).toDouble / Shibetoshi

        return (date, amount, fee)
      }
    }
  }

  def dogeScrap2(url : String) : (Date, Double, Double) = {
    val str = BlockExplorerSearcher.fromURL(url)

    val feeStr = locateAndSkip(str, "<td>Fee</td>", '>', 1, "<")
    val fee = Parse.asDouble(feeStr.replace("<small>", "").replace("</small>", ""))

    val dateStr = locateAndSkip(str, "Appeared in block", '>', 2, "<")
    val date = Date.fromString(dateStr.replace(" at ", ""), "yyyy-MM-dd hh:mm:ss z")
    return (date, 0, fee)
  }


  def vtcScrap2(url : String) : (Date, Double, Double) = {
    val str = BlockExplorerSearcher.fromURL(url)

    val feeStr = locateAndSkip(str, "<th>Fees</th>", '>', 1, "V")
    val fee = Parse.asDouble(feeStr.replace("<span class='muteds'>", "").replace("</span>", ""))

    val dateStr = locateAndSkip(str, "<th>Time</th>", '>', 1, "<")
    val date = Date.fromString(dateStr.replace(" at ", ""), "yyyy-MM-dd hh:mm:ss z")
    return (date, 0, fee)
  }
}

class BlockExplorerSearcher(market : Market, txid : String, address : String) {
  val (date, amount, fee) = market match {
    case Market.bitcoin  => BlockExplorerSearcher.btcScrap(txid, address)
    case Market.etc      => BlockExplorerSearcher.etcScrap(txid, address)
    case Market.dogecoin => BlockExplorerSearcher.dogeScrap(txid, address)
    case Market.litecoin => BlockExplorerSearcher.ltcScrap(txid, address)
    case Market.vertcoin => BlockExplorerSearcher.vtcScrap(txid, address)
    case _               => Logger.fatal("BlockExplorerScraper: non-supported market %s".format(market))
  }
}


