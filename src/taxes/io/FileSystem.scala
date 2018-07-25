package taxes.io

import java.nio.charset._

import taxes.exchanger.Exchanger
import taxes.util.Logger
import taxes.{Config, Market}


object FileSystem {
  type File = java.io.File

  object File {
    def apply(fileName : String) : File =
      new File(fileName)
  }

  val data = "data"

  def pathFromData(fullPath : String) : String = {
    val i = fullPath.lastIndexOf(data)
    if(i>=0)
      fullPath.drop(i)
    else
      fullPath
  }

  val prices = s"$data/prices"

  val coinMarketCap = s"$prices/coinmarketcap"

  val euroUSDFile = s"$prices/euroUSD/tc_1_1.csv"

  val coinMarketCapExtension = ".json"

  def coinMarketCapExtension(year : Int) : String = s".$year$coinMarketCapExtension"

  def coinMarketCapFolder(market : Market) =
    s"$coinMarketCap/$market"

  def coinMarketCapFile(market : Market, year : Int) =
    s"${coinMarketCapFolder(market)}/$market${coinMarketCapExtension(year)}"

  lazy val userPersistanceFolder = s"$usr/${Config.config.user}/persistance"

  lazy val transactionsCacheFile = s"$userPersistanceFolder/transactions.cache.json"

  def userPersistanceFolder(year : Int) : String =
    s"$userPersistanceFolder/$year"

  def marketFile(year : Int) =
    s"${userPersistanceFolder(year)}/market.json"

  def configFile(year : Int) = s"${userPersistanceFolder(year)}/config.json"

  def operationsFile(year : Int) = s"${userPersistanceFolder(year)}/operations.json"

  def processedOperationsFile(year : Int) = s"${userPersistanceFolder(year)}/processed.json"

  def stocksFolder(year : Int) = s"${userPersistanceFolder(year)}/stocks"
  def stocksLedgerFolder(year : Int) = s"${stocksFolder(year)}/ledger"
  val stockExtension = ".json"
  def stockFile(year : Int, id : String) = s"$id$stockExtension"
  def stockLedgerFile(year : Int, id : String) = s"${stocksLedgerFolder(year)}/$id$stockExtension"

  def marginFolder(year : Int, exchanger : Exchanger, isBuys : Boolean) =
    s"${userPersistanceFolder(year)}/margin/$exchanger/${if (isBuys) "buys" else "sells"}"
  def marginLedgerFolder(year : Int, exchanger : Exchanger, isBuys : Boolean) =
    s"${marginFolder(year, exchanger, isBuys)}/ledger"
  def marginFile(year : Int, exchanger : Exchanger, isBuys : Boolean, id : String) =
    s"${marginFolder(year, exchanger, isBuys)}/$id$stockExtension"
  def marginLedgerFile(year : Int, exchanger : Exchanger, isBuys : Boolean, id : String) =
    s"${marginLedgerFolder(year, exchanger, isBuys)}/$id$stockExtension"

  val usr = s"$data/usr"
  lazy val userInputFolder = s"$usr/${Config.config.user}/input"
  lazy val userOutputFolder = s"$usr/${Config.config.user}/output"

  def userOutputFolder(year : Int): String =
    s"$userOutputFolder/$year"

  val readConfigFolder = s"$data/config"
  def readConfigFile(fileName : String) = s"$readConfigFolder/$fileName"


  def backup(file : File): Unit = {
    val (folder, name, ext) = decompose(file)

    val backupFolder = s"$folder/backup"
    mkDir(backupFolder)

    val fileName = name+ext

    if(file.exists()) {
      Logger.trace(s"Backing up file $file.")
      var i = 0
      var ok = false
      var backup : File = null
      while(!ok) {
        i += 1
        backup = new File(s"$backupFolder/$fileName.bak$i")
        ok = !backup.exists()
      }
      file.renameTo(backup)
    }
  }

  def findFilesAt(path : String, extension : String) : Array[File] = {
    val f = new File(path)
    val matchingFiles = f.listFiles(new java.io.FilenameFilter() {
      def accept(dir: File, name: String): Boolean =
        !name.startsWith("readme") && name.endsWith(extension)
    })
    return matchingFiles
  }

  def mkDir(path : String): Unit = {
    val folder = new File(path)
    if (!folder.exists())
      folder.mkdirs()
  }

  def getParent(path : String) : String =
    decompose(path)._1

  def getName(path : String) : String =
    decompose(path)._2

  def getExtension(path : String) : String =
    decompose(path)._3

  def decompose(file : File) : (String, String, String) = {
    val name = file.getName
    val idx = name.lastIndexOf(".")
    return(file.getParent, name.substring(0, idx), name.substring(idx))
  }

  def decompose(path : String) : (String, String, String) =
    decompose(new File(path))

  def compose(paths : Seq[String], name : String, ext : String) : String = {
    val ext1 = if(ext.head=='.') ext else "."+ext
    return s"${paths.mkString("/")}/$name$ext1"
  }

  type PrintStream = java.io.PrintStream

  object PrintStream {
    private def _apply(file : File, charset : Charset = defaultCharset, doBackUp : Boolean = true): java.io.PrintStream = {
      val path = file.getParentFile

      if(!path.exists())
        path.mkdirs()

      if(doBackUp)
        backup(file)
      new java.io.PrintStream(file, charset.name())
    }

    def apply(file : File, charset : Charset, doBackUp : Boolean): java.io.PrintStream =
      _apply(file, charset, doBackUp)

    def apply(file : File, charset : Charset): java.io.PrintStream =
      _apply(file, charset = charset)

    def apply(file : File, doBackUp : Boolean): java.io.PrintStream =
      _apply(file, doBackUp = doBackUp)

    def apply(file : File): java.io.PrintStream =
      _apply(file)

    def apply(fileName : String, charset : Charset, doBackUp : Boolean): java.io.PrintStream =
      _apply(new File(fileName), charset, doBackUp)

    def apply(fileName : String, charset : Charset): java.io.PrintStream =
      _apply(new File(fileName), charset = charset)

    def apply(fileName : String, doBackUp : Boolean): java.io.PrintStream =
      _apply(new File(fileName), doBackUp = doBackUp)

    def apply(fileName : String): java.io.PrintStream =
      _apply(new File(fileName))
  }


  private def _withPrintStream(file : File, charset : Charset = defaultCharset)(p : java.io.PrintStream => Unit): Unit = {
    var ps : java.io.PrintStream = null
    try {
      ps = PrintStream(file, charset)
      p(ps)
    } finally {
      if (ps != null)
        ps.close()
    }
  }

  def withPrintStream(file : File, charset : Charset)(p : java.io.PrintStream => Unit): Unit =
    _withPrintStream(file, charset)(p)

  def withPrintStream(file : File)(p : java.io.PrintStream => Unit): Unit =
    _withPrintStream(file)(p)

  def withPrintStream(fileName : String, charset : Charset)(p : java.io.PrintStream => Unit): Unit =
    _withPrintStream(new File(fileName), charset)(p)

  def withPrintStream(fileName : String)(p : java.io.PrintStream => Unit): Unit =
    _withPrintStream(new File(fileName))(p)


  type Source = scala.io.Source

  private def _withSource[A](file : File, charset: Charset = defaultCharset)(p : scala.io.Source => A) : A = {
    var src : scala.io.Source = null
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

  def withSource[A](file : File, charset: Charset)(p : scala.io.Source => A) : A =
    _withSource(file, charset)(p)

  def withSource[A](file : File)(p : scala.io.Source => A) : A =
    _withSource(file)(p)

  def withSource[A](fileName : String, charset: Charset)(p : scala.io.Source => A) : A =
    withSource(new File(fileName), charset)(p)

  def withSource[A](fileName : String)(p : scala.io.Source => A) : A =
    withSource(new File(fileName))(p)
}
