package taxes

import taxes.io.FileSystem
import taxes.util.Logger
import taxes.util.parse.Parse
import spray.json.JsonProtocol._

import scala.collection.mutable.ListBuffer

object RichText {
  implicit val richTextJson = jsonFormat1(RichText.apply)

  private val tkBegin = "<"
  private val tkEnd   = ">"
  private val tkSep   = "|"

  private val tkAddress = "address"
  private val tkBold = "bold"
  private val tkTransaction = "transaction"
  private val tkURL = "url"
  private val tkNl = "nl"
  private val tkReport = "report"
  private val tkSmall = "small"


  type Elem = String

  def address(currency: Currency, addr: String): Elem =
    s"$tkBegin$tkAddress$tkSep$currency$tkSep$addr$tkEnd"

  def bold(text: String): Elem =
    s"$tkBegin$tkBold$tkSep$text$tkEnd"

  def small(text: String): Elem =
    s"$tkBegin$tkSmall$tkSep$text$tkEnd"

  def transaction(currency: Currency, txid: String): Elem =
    s"$tkBegin$tkTransaction$tkSep$currency$tkSep$txid$tkEnd"

  def url(text: String, url: String): Elem =
    s"$tkBegin$tkURL$tkSep$text$tkSep$url$tkEnd"

  def nl: Elem =
    s"$tkBegin$tkNl$tkEnd"

  def report(year: Int, operationNumber: Int, showYear: Boolean=false): Elem =
    if(showYear)
      s"$tkBegin$tkReport$tkSep$year$tkSep$operationNumber$tkSep$showYear$tkEnd"
    else
      s"$tkBegin$tkReport$tkSep$year$tkSep$operationNumber$tkEnd"

  private def parseElem(richElem: Elem): List[String] = {
    Parse.removePrefix(richElem, tkBegin) match {
      case None =>
        List(richElem) // not a command. Just some text
      case Some(rest1) =>
        Parse.removeSuffix(rest1, tkEnd) match {
          case None =>
            Logger.fatal(s"RichText.parseItem: $tkEnd expected but not found in $richElem")
          case Some(rest2) =>
            val idx = rest2.indexOf(tkSep)
            if (idx < 0)
              List(rest2) // a command without args
            else {
              val (command, rest3) = rest2.splitAt(idx)
              val Some(rest4) = Parse.removePrefix(rest3, tkSep)
              val args = Parse.sepBy(rest4, tkSep)
              command :: args
            }
        }
    }
  }

  private def elemToString(richElem: Elem): String = parseElem(richElem) match {
    case List(`tkAddress`, currency, addr) =>
      addr
    case List(`tkBold`, text) =>
      text
    case List(`tkSmall`, text) =>
      text
    case List(`tkTransaction`, currency, txid) =>
      txid
    case List(`tkURL`, text, url) =>
      text
    case List(`tkNl`) =>
      "\n"
    case List(`tkReport`, year, operationNumber) =>
      operationNumber.toString
    case List(`tkReport`, year, operationNumber, showYear) =>
      if(showYear=="true") s"$operationNumber ($year)" else s"$operationNumber"
    case _ =>
      richElem
  }

  private def elemToHTML(richElem: Elem): HTML = parseElem(richElem) match {
    case List(`tkAddress`, currency, addr) =>
      val maxLen = 45
      val truncatedAddr = if(addr.length>maxLen) addr.take(maxLen) + "..." else addr
      BlockExplorer.addressURL(currency, addr) match {
        case None => <span class='addr'>{truncatedAddr}</span>
        case Some(url) => <a href={s"$url"} class='addr'>{truncatedAddr}</a>
      }
    case List(`tkBold`, text) =>
      <b>{text}</b>
    case List(`tkSmall`, text) =>
      <span class="small1">{text}</span>
    case List(`tkTransaction`, currency, txid) =>
      val maxLen = 70
      val truncatedTxid = if(txid.length>maxLen) txid.take(maxLen) + "..." else txid
      BlockExplorer.transactionURL(currency, txid) match {
        case None => <span class='tx'>{truncatedTxid}</span>
        case Some(url) => <a href={s"$url"} class='tx'>{truncatedTxid}</a>
      }
    case List(`tkURL`, text, url) =>
      <a href={s"$url"} class='noDecor'>{text}</a>
    case List(`tkNl`) =>
      <br/>
    case List(`tkReport`, _year, _operationNumber) =>
      val year = Parse.asInt(_year)
      val operationNumber = Parse.asInt(_operationNumber)
      <a href={s"${FileSystem.linkToReportHTML(year, operationNumber)}"} class='opNumber'>{operationNumber}</a>
    case List(`tkReport`, _year, _operationNumber, showYear) =>
      val year = Parse.asInt(_year)
      val operationNumber = Parse.asInt(_operationNumber)
      <a href={s"${FileSystem.linkToReportHTML(year, operationNumber)}"} class='opNumber'>
        {if(showYear=="true") s"$operationNumber ($year)" else operationNumber}
      </a>
    case _ =>
      <span>{richElem}</span>
  }


  // Simple parser. Assumes no nesting of commands
  private def parseString(str: String): List[Elem] = {
    val items = ListBuffer[Elem]()
    var end = false
    var richText1 = str
    while(!end && richText1.nonEmpty) {
      val idx1 = richText1.indexOf(tkBegin)
      if(idx1 < 0) {
        items.append(richText1)
        end = true
      } else {
        if(idx1 > 0) {
          val before = richText1.take(idx1)
          items.append(before)
        }
        val idx2 = richText1.indexOf(tkEnd)
        val item = richText1.substring(idx1, idx2+1)
        items.append(item)
        richText1 = richText1.drop(idx2+1)
      }
    }
    items.toList
  }

  object util {
    def address(currency: Currency, address: String): Elem =
      s"${AddressBook.richElem(currency, address)}"

    def transaction(currency: Currency, txid: String, address: String): Elem =
      s"${AddressBook.richElem(currency, address)}${RichText.nl}${RichText.transaction(currency, txid)}"

    def transaction(currency: Currency, txid: String): Elem =
      s"${RichText.transaction(currency, txid)}"

    def transaction(currency: Currency, txid: Option[String], address: Option[String]): Elem =
      (txid, address) match {
        case (Some(hash), Some(adrr)) =>
          util.transaction(currency, hash, adrr)
        case (Some(hash), None) =>
          util.transaction(currency, hash)
        case (None, Some(adrr)) =>
          util.address(currency, adrr)
        case (None, None) =>
          ""
      }


    def onlineExchange(inCurrency: Currency, inTxid: String, outCurrency: Currency, outTxid: String): Elem = {
      val in = BlockExplorer.transactionURL(inCurrency, inTxid) match {
          case None => inCurrency
          case Some(url) => RichText.url(inCurrency, url)
      }

      val out = BlockExplorer.transactionURL(outCurrency, outTxid) match {
        case None => outCurrency
        case Some(url) => RichText.url(outCurrency, url)
      }

      return s"($in ➞ $out)"
    }
  }
}


case class RichText(str: String) extends Iterable[Char] {
  import RichText._

  def toHTML: HTML =
    <span>{parseString(str).map(elemToHTML)}</span>

  override def toString: String =
    parseString(str).map(elemToString).mkString

  override def iterator: Iterator[Char] =
    str.iterator

  def +(that: RichText): RichText =
    RichText(this.str + that.str)

  def +(that: String): RichText =
    RichText(this.str + that)
}