package taxes

import taxes.date._
import taxes.io.Network
import taxes.util.Logger
import taxes.util.parse.Parse


object BlockExplorerSearcher {
  def apply(currency: Currency, txid: String, address: String) =
    new BlockExplorerSearcher(currency, txid, address)

  private def locateAndSkip(str: String, prefix: String, toSkip: Char, numSkip: Int, endToken: String): String = {
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

  def etcScrap(txid: String, address: String): (LocalDateTime, Double, Double) = {
    val url = s"https://gastracker.io/tx/$txid"
    val str = Network.Http.withSource(url){
      src => src.mkString
    }

    val dateStr = locateAndSkip(str, "<dt>Timestamp</dt>", '>', 1, "</dd>")
    val date = LocalDateTime.parse(dateStr, "EE MMM dd HH:mm:ss zzz yyyy")

    val amountStr = locateAndSkip(str, "<dt>Value</dt>", '>', 1, " Ether")
    val amount = Parse.asDouble(amountStr)

    val gasUsedStr = locateAndSkip(str, "<dt>Gas Used</dt>", '>', 1, "</dd>")
    val gasUsed = Parse.asInt(gasUsedStr)

    val gasPriceStr = locateAndSkip(str, "<dt>Gas Price</dt>", '>', 1, " Mwei")
    val gasPrice = Parse.asInt(gasPriceStr)

    val addressStr = locateAndSkip(str, "<dt>To</dt>", '>', 2, "</a>")
    if(address.toUpperCase != addressStr.toUpperCase())
      Logger.fatal(s"BlockExplorerScraper.etcScrap: address doesn't match $address $addressStr")

    val fee = gasPrice * gasUsed * 1E-12

    return (date, amount, fee)
  }


  def etcScrap2(txid: String, address: String): (LocalDateTime, Double, Double) = {
    import spray.json._
    import spray.json.JsonProtocol._

    def JsonETCQuery[A](method: String, params: JsArray)(implicit json: JsonFormat[A]): A = {
      val RPC_API_endPoint = "http://web3.gastracker.io"
      val response = Network.Http.Json.RPC(RPC_API_endPoint, method = method, params = params)
      return response.convertTo[A]
    }

    case class Transaction(blockHash: String, blockNumber: String, from: String, to: String, gas: String, gasPrice: String, value: String)
    implicit val transactionJson = jsonFormat7(Transaction)

    case class Block(timestamp: String)
    implicit val blockJson = jsonFormat1(Block)

    case class TransactionReceipt(gasUsed: String, cumulativeGasUsed: String)
    implicit val transactionReceiptJson = jsonFormat2(TransactionReceipt)

    val tx = JsonETCQuery[Transaction]("eth_getTransactionByHash", JsArray(JsString(txid)))

    if(tx.to.toUpperCase != address.toUpperCase())
      Logger.fatal(s"BlockExplorerScraper.etcScrap: address doesn't match $address ${tx.to}")

    val block = JsonETCQuery[Block]("eth_getBlockByHash", JsArray(JsString(tx.blockHash), JsBoolean(true)))

    val txReceipt = JsonETCQuery[TransactionReceipt]("eth_getTransactionReceipt", JsArray(JsString(txid)))

    def parseHex(str: String): BigInt = {
      val toParse = if(str.startsWith("0x")) str.drop(2) else str
      BigInt(toParse, 16)
    }

    def weiToGwei(x: BigInt): BigInt = x / BigInt(10).pow(9)

    def weiToETC(x: BigInt): Double =
      weiToGwei(x).toDouble / 1E9

    val date = LocalDateTime.fromUnix(parseHex(block.timestamp).toLong)
    val amount = weiToETC(parseHex(tx.value))
    val fee = weiToETC(parseHex(txReceipt.gasUsed) * parseHex(tx.gasPrice))

    return (date, amount, fee)
  }

  def chainzCryptoidInfoScrap(coin: String, txid: String, address: String): (LocalDateTime, Double, Double) = {
    import spray.json._
    import spray.json.JsonProtocol._

    case class Output(addr: String, amount: Double)
    implicit val outputJson = jsonFormat2(Output)

    case class Response(timestamp: Long, fees: Double, outputs: Seq[Output])
    implicit val responseJson = jsonFormat3(Response)

    val url = s"https://chainz.cryptoid.info/$coin/api.dws?q=txinfo;t=$txid"
    val resp = Network.Http.withSource(url){src => src.mkString}

    val json = resp.parseJson
    val response = json.convertTo[Response]

    val date = LocalDateTime.fromUnix(response.timestamp)
    val fee = response.fees

    var found = false
    val it = response.outputs.iterator
    var amount = 0.0
    while(!found && it.hasNext) {
      val output = it.next()
      if(output.addr == address) {
        amount = output.amount
        found = true
      }
    }

    if(!found)
      Logger.fatal(s"BlockExplorerScraper.chainzCryptoidInfoScrap: output address $address not found in JSON $response")

    return (date, amount, fee)
  }

  def btcScrap(txid: String, address: String): (LocalDateTime, Double, Double) =
    chainzCryptoidInfoScrap("btc", txid, address)

  def ltcScrap(txid: String, address: String): (LocalDateTime, Double, Double) =
    chainzCryptoidInfoScrap("ltc", txid, address)

  def vtcScrap(txid: String, address: String): (LocalDateTime, Double, Double) =
    chainzCryptoidInfoScrap("vtc", txid, address)

  def dogeScrap(txid: String, address: String): (LocalDateTime, Double, Double) = {
    import spray.json._
    import spray.json.JsonProtocol._

    case class Input(address: String, value: String)
    implicit val inputJson = jsonFormat2(Input)

    case class Output(address: String, value: String)
    implicit val outputJson = jsonFormat2(Output)

    case class Transaction(time: Long, inputs: Seq[Input], outputs: Seq[Output])
    implicit val transactionJson = jsonFormat3(Transaction)

    case class Response(success: Int, transaction: Transaction)
    implicit val responseJson = jsonFormat2(Response)

    val url = s"https://dogechain.info/api/v1/transaction/$txid"
    val resp = Network.Http.withSource(url){src => src.mkString}

    val json = resp.parseJson
    val response = json.convertTo[Response]

    if(response.success != 1)
      Logger.fatal(s"BlockExplorerScraper.dogeScrap: could not find transaction $txid")

    val transaction = response.transaction

    val date = LocalDateTime.fromUnix(transaction.time)
    val outputs = transaction.outputs

    var amount = 0.0
    var found = false

    var sumOuts = BigDecimal(0)
    for(output <- outputs) {
      val am = Parse.asBigDecimal(output.value)
      sumOuts += am

      if(output.address == address) {
        amount = am.doubleValue()
        found = true
      }
    }

    if(!found)
      Logger.fatal(s"BlockExplorerScraper.dogeScrap: output address $address not found in JSON $response")

    val inputs = transaction.inputs

    var sumIns = BigDecimal(0)
    for(input <- inputs) {
      val am = Parse.asDouble(input.value)
      sumIns += am
    }

    val fee = (sumIns - sumOuts).doubleValue()

    return (date, amount, fee)
  }

  def btcScrap2(txid: String, address: String): (LocalDateTime, Double, Double) = {
    val url = s"https://blockchain.info/tx/$txid"
    val str = Network.Http.withSource(url){src => src.mkString}

    val dateStr = Parse.trimSpaces(locateAndSkip(str, "<td>Received Time</td>", '>', 1, "</td>"))
    val date = LocalDateTime.parseAsUTC(dateStr, "yyyy-MM-dd HH:mm:ss")

    val amountStr = locateAndSkip(str, s"$address</a>", '>', 2, " BTC")
    val amount = Parse.asDouble(amountStr)

    val feeStr = locateAndSkip(str, "<td>Fees</td>", '>', 2, " BTC")
    val fee = Parse.asDouble(feeStr)

    return (date, amount, fee)
  }

  def dogeScrap2(url: String): (LocalDateTime, Double, Double) = {
    val str = Network.Http.withSource(url){src => src.mkString}

    val feeStr = locateAndSkip(str, "<td>Fee</td>", '>', 1, "<")
    val fee = Parse.asDouble(feeStr.replace("<small>", "").replace("</small>", ""))

    val dateStr = locateAndSkip(str, "Appeared in block", '>', 2, "<")
    val date = LocalDateTime.parse(dateStr.replace(" at ", ""), "yyyy-MM-dd hh:mm:ss z")
    return (date, 0, fee)
  }


  def vtcScrap2(url: String): (LocalDateTime, Double, Double) = {
    val str = Network.Http.withSource(url){src => src.mkString}

    val feeStr = locateAndSkip(str, "<th>Fees</th>", '>', 1, "V")
    val fee = Parse.asDouble(feeStr.replace("<span class='muteds'>", "").replace("</span>", ""))

    val dateStr = locateAndSkip(str, "<th>Time</th>", '>', 1, "<")
    val date = LocalDateTime.parse(dateStr.replace(" at ", ""), "yyyy-MM-dd hh:mm:ss z")
    return (date, 0, fee)
  }
}

class BlockExplorerSearcher(currency: Currency, txid: String, address: String) {
  lazy val search = currency match {
    case Currency.bitcoin  => Some(BlockExplorerSearcher.btcScrap(txid, address))
    case Currency.dogecoin => Some(BlockExplorerSearcher.dogeScrap(txid, address))
    case Currency.etc      => Some(BlockExplorerSearcher.etcScrap(txid, address))
    case Currency.litecoin => Some(BlockExplorerSearcher.ltcScrap(txid, address))
    case Currency.vertcoin => Some(BlockExplorerSearcher.vtcScrap(txid, address))
    case _                 => None
  }

  lazy val (date, amount, fee) = search match {
    case Some(t3) => t3
    case None => Logger.fatal(s"BlockExplorerScraper: non-supported currency $currency")
  }
}
