package taxes.io

import java.nio.charset._

import taxes.exchanger.Exchanger
import taxes.util.Logger
import taxes.{Config, Currency}


object FileSystem {
  type File = java.io.File

  def looksLikeUTF16LE(fileName: String): (Boolean, Boolean) = {
    var is: java.io.FileInputStream = null
    var isUTF16LE = false
    var hasBOM = false
    try {
      is = new java.io.FileInputStream(fileName)
      val sz = 10
      val buffer = new Array[Byte](sz)
      val readSz = is.read(buffer, 0, sz)

      hasBOM =
        if(readSz>1)
          buffer(0) == 0xFF.toByte && buffer(1) == 0xFE.toByte
        else
          false

      isUTF16LE = readSz > 1
      var i = if(hasBOM) 2 else 0
      while(isUTF16LE && i < readSz) {
        isUTF16LE = isUTF16LE && (if(i % 2 == 0) buffer(i) != 0 else buffer(i) == 0)
        i += 1
      }
    } catch {
      case _ =>
        ;
    } finally {
      if(is != null)
        is.close()
    }
    return (isUTF16LE, hasBOM)
  }

  object File {
    def apply(fileName: String): File =
      new File(fileName)
  }

  val data = "data"

  def pathFromData(fullPath: String): String = {
    val i = fullPath.lastIndexOf(data)
    if(i>=0)
      fullPath.drop(i)
    else
      fullPath
  }

  val pricesFolder = s"$data/prices"

  val euroUSDFile = s"$pricesFolder/euroUSD/tc_1_1.csv"

  val coinMarketCap = s"$pricesFolder/coinmarketcap"
  val coinMarketCapExtension = ".json"

  def coinMarketCapExtension(year: Int): String = s".USD.$year$coinMarketCapExtension"

  def coinMarketCapFolder(currency: Currency) =
    s"$coinMarketCap/$currency"

  def coinMarketCapFile(currency: Currency, year: Int) =
    s"${coinMarketCapFolder(currency)}/$currency${coinMarketCapExtension(year)}"


  val coinGecko = s"$pricesFolder/coingecko"
  val coinGeckoExtension = ".json"

  def coinGeckoExtension(year: Int): String = s".USD.$year$coinGeckoExtension"

  def coinGeckoFolder(currency: Currency) =
    s"$coinGecko/$currency"

  def coinGeckoFile(currency: Currency, year: Int) =
    s"${coinGeckoFolder(currency)}/$currency${coinGeckoExtension(year)}"


  lazy val userPersistenceFolder = s"$usrFolder/${Config.config.user}/persistence"

  lazy val transactionsCacheFile = s"$userPersistenceFolder/transactions.cache.json"

  def userPersistenceFolder(year: Int): String =
    s"$userPersistenceFolder/$year"

  def currencyFile(year: Int) =
    s"${userPersistenceFolder(year)}/currency.json"

  def configFile(year: Int) = s"${userPersistenceFolder(year)}/config.json"

  def operationsFile(year: Int) = s"${userPersistenceFolder(year)}/operations.json"

  def processedOperationsFile(year: Int) = s"${userPersistenceFolder(year)}/processed.json"

  val ledgerExtension = ".json"
  val ledgerPoolExtension = ".json"

  def stocksFolder(year: Int) = s"${userPersistenceFolder(year)}/stocks"
  def stocksLedgerFolder(year: Int) = s"${stocksFolder(year)}/ledger"
  val stockExtension = ".json"
  def stockFile(year: Int, id: String) = s"$id$stockExtension"
  def stockLedgerFile(year: Int, id: String) = s"${stocksLedgerFolder(year)}/$id$stockExtension"

  def exchangersFolder(year: Int) = s"${userPersistenceFolder(year)}/exchangers"
  def exchangerLedgersFolder(year: Int, exchanger: Exchanger) =
    s"${exchangersFolder(year)}/$exchanger/ledger"
  def exchangerMarginFolder(year: Int, exchanger: Exchanger, isLong: Boolean) =
    s"${exchangersFolder(year)}/$exchanger/margin/${if(isLong) "longs" else "shorts"}"
  def exchangerMarginLedgerFolder(year: Int, exchanger: Exchanger, isBuys: Boolean) =
    s"${exchangerMarginFolder(year, exchanger, isBuys)}/ledger"
  def exchangerMarginFile(year: Int, exchanger: Exchanger, isBuys: Boolean, id: String) =
    s"${exchangerMarginFolder(year, exchanger, isBuys)}/$id$stockExtension"
  def exchangerMarginLedgerFile(year: Int, exchanger: Exchanger, isBuys: Boolean, id: String) =
    s"${exchangerMarginLedgerFolder(year, exchanger, isBuys)}/$id$stockExtension"


  val usrFolder = s"$data/usr"
  lazy val userInputFolder = s"$usrFolder/${Config.config.user}/input"
  lazy val userOutputFolder = s"$usrFolder/${Config.config.user}/output"

  def userOutputFolder(year: Int): String =
    s"$userOutputFolder/$year"

  private def reportName(year: Int, extension: String): String = {
    val method = taxes.Accounting.toString(Config.config.accountingMethod)
    s"$method.$year.$extension"
  }

  def report(year: Int, extension: String = "html"): String =
    s"${FileSystem.userOutputFolder(year)}/${reportName(year, extension)}"

  def linkToReportHTML(year: Int, operationNumber: Int): String =
    s"../$year/${reportName(year, "html")}#$operationNumber"

  val configFolder = s"$data/config"
  def inConfigFolder(fileName: String) = s"$configFolder/$fileName"

  val addressBookFolder = s"$userInputFolder/addressBook"

  val filtersFolder = s"$userInputFolder/filters"

  def backup(file: File): Unit = {
    val (folder, name, ext) = decompose(file)

    val backupFolder = s"$folder/backup"
    mkDir(backupFolder)

    val fileName = name+ext

    if(file.exists()) {
      Logger.trace(s"Backing up file $file.")
      var i = 0
      var ok = false
      var backup: File = null
      while(!ok) {
        i += 1
        backup = new File(s"$backupFolder/$fileName.bak$i")
        ok = !backup.exists()
      }
      file.renameTo(backup)
    }
  }

  def findFilesAt(path: String, extension: String): Array[File] = {
    val f = new File(path)
    val matchingFiles = f.listFiles(new java.io.FilenameFilter() {
      def accept(dir: File, name: String): Boolean =
        !name.startsWith("readme") && name.endsWith(extension)
    })
    return matchingFiles
  }

  def mkDir(path: String): Unit = {
    val folder = new File(path)
    if(!folder.exists())
      folder.mkdirs()
  }

  def getParent(path: String): String =
    decompose(path)._1

  def getName(path: String): String =
    decompose(path)._2

  def getExtension(path: String): String =
    decompose(path)._3

  def decompose(file: File): (String, String, String) = {
    val name = file.getName
    val idx = name.lastIndexOf(".")
    if(idx<0)
      (file.getParent, name, "")
    else
      (file.getParent, name.substring(0, idx), name.substring(idx))
  }

  def decompose(path: String): (String, String, String) =
    decompose(new File(path))

  def compose(paths: Seq[String], name: String, ext: String): String = {
    val ext1 = if(ext.head=='.') ext else "."+ext
    return s"${paths.mkString("/")}/$name$ext1"
  }

  def matchesYear(path: String, yearFilter: Option[Int]): Boolean =
    yearFilter match {
      case None =>
        true // no filter; anyone matches
      case Some(year) =>
        val (_, name, ext) = FileSystem.decompose(path)
        val (_, _, ext2) = FileSystem.decompose(name)
        if(ext2.isEmpty)
          true // no year in file name
        else {
          val ext3 = ext2.tail
          if (ext3.contains("-")) {
            val (str1, str2) = taxes.util.parse.Parse.split(ext3, "-")
            try {
              val yearBegin = taxes.util.parse.Parse.asInt(str1)
              val yearEnd = taxes.util.parse.Parse.asInt(str2)
              // filter in file name is a range
              yearBegin <= year && year <= yearEnd
            } catch {
              case _ => true // it's not a range
            }
          } else
            try {
              val fileYear = taxes.util.parse.Parse.asInt(ext3)
              // filter in file name is a single year
              fileYear == year
            } catch {
              case _ => true // it's something else
            }
        }
    }

  type PrintStream = java.io.PrintStream

  object PrintStream {
    def apply(file: File, charset: Charset = defaultCharset, doBackUp: Boolean = true): java.io.PrintStream = {
      val path = file.getParentFile

      if(!path.exists())
        path.mkdirs()

      if(doBackUp)
        backup(file)
      new java.io.PrintStream(file, charset.name())
    }

    def apply(fileName: String, charset: Charset, doBackUp: Boolean): java.io.PrintStream =
      apply(new File(fileName), charset, doBackUp)

    def apply(fileName: String, charset: Charset): java.io.PrintStream =
      apply(new File(fileName), charset = charset)

    def apply(fileName: String, doBackUp: Boolean): java.io.PrintStream =
      apply(new File(fileName), doBackUp = doBackUp)

    def apply(fileName: String): java.io.PrintStream =
      apply(new File(fileName))
  }


  def withPrintStream(file: File, charset: Charset = defaultCharset, doBackUp: Boolean = true)(p: java.io.PrintStream => Unit): Unit = {
    var ps: java.io.PrintStream = null
    try {
      ps = PrintStream(file, charset, doBackUp)
      p(ps)
    } finally {
      if(ps != null)
        ps.close()
    }
  }

  def withPrintStream(fileName: String, charset: Charset, doBackUp: Boolean)(p: java.io.PrintStream => Unit): Unit =
    withPrintStream(new File(fileName), charset, doBackUp)(p)

  def withPrintStream(fileName: String, charset: Charset)(p: java.io.PrintStream => Unit): Unit =
    withPrintStream(new File(fileName), charset)(p)

  def withPrintStream(fileName: String, doBackUp: Boolean)(p: java.io.PrintStream => Unit): Unit =
    withPrintStream(new File(fileName), doBackUp = doBackUp)(p)

  def withPrintStream(fileName: String)(p: java.io.PrintStream => Unit): Unit =
    withPrintStream(new File(fileName))(p)


  type Source = scala.io.Source

  def withSource[A](file: File, charset: Charset = defaultCharset)(p: scala.io.Source => A): A = {
    var src: scala.io.Source = null
    try {
      src = scala.io.Source.fromFile(file, charset.name())
      val x = p(src)
      return x
    } finally {
      // toDo if src escapes p procedure scope, we will close connection before reading from it
      if(src != null)
        src.close()
    }
  }

  def withSource[A](fileName: String, charset: Charset)(p: scala.io.Source => A): A =
    withSource(new File(fileName), charset)(p)

  def withSource[A](fileName: String)(p: scala.io.Source => A): A =
    withSource(new File(fileName))(p)
}
