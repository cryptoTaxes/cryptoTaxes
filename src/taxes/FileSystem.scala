package taxes

import java.io.File

import taxes.exchanger.Exchanger


object FileSystem extends Initializable {
  val data = "data"

  def pathFromData(fullPath : String) : String = {
    val i = fullPath.indexOf(data)
    if(i>=0)
      fullPath.drop(i)
    else
      fullPath
  }

  val prices = data+"/prices"

  val coinMarketCap = prices+"/coinmarketcap"

  val euroUSDFile = prices+"/euroUSD/tc_1_1.csv"

  def coinMarketCapFile(market : Market) =
    coinMarketCap+"/"+market+".txt"

  def coinMarketCapHtmlFile(market : Market) =
    coinMarketCap+"/"+market+".html"

  def backup(fileName : String): Unit = {
    val file = new java.io.File(fileName)
    if(file.exists()) {
      var i = 0
      var ok = false
      var backup : java.io.File = null
      while(!ok) {
        i += 1
        backup = new java.io.File(fileName + ".bak"+i)
        ok = !backup.exists()
      }
      file.renameTo(backup)
    }
  }

  val usr = data+"/usr"
  lazy val userInputFolder = usr+"/"+Config.config.user+"/input"
  lazy val userOutputFolder = usr+"/"+Config.config.user+"/output"

  def userOutputFolder(year : Int): String =
    userOutputFolder+"/"+year

  val conf = data+"/config"
  def configFile(fileName : String) = conf+"/"+fileName

  def findFilesAt(path : String, extension : String) : Array[java.io.File] = {
    val f = new java.io.File(path)
    val matchingFiles = f.listFiles(new java.io.FilenameFilter() {
      def accept(dir: java.io.File, name: String): Boolean =
        !name.startsWith("readme") && name.endsWith(extension)
    })
    return matchingFiles
  }

  lazy val userPersistanceFolder = usr+"/"+Config.config.user+"/persistance"

  lazy val transactionsCacheFile = userPersistanceFolder+"/"+"transactions.cache.json"

  def userPersistanceFolder(year : Int) : String =
    userPersistanceFolder+"/"+year

  def stocksFolder(year : Int) = userPersistanceFolder(year)+"/"+"stocks"
  def stocksLedgerFolder(year : Int) = stocksFolder(year)+"/"+"ledger"
  val stockExtension = ".json"
  def stockFile(year : Int, id : String) = id+stockExtension
  def stockLedgerFile(year : Int, id : String) = stocksLedgerFolder(year)+"/"+id+stockExtension


  def marginFolder(year : Int, exchanger : Exchanger, isBuys : Boolean) = userPersistanceFolder(year)+"/"+"margin"+"/"+exchanger+"/"+(if(isBuys) "buys" else "sells")
  def marginLedgerFolder(year : Int, exchanger : Exchanger, isBuys : Boolean) = marginFolder(year, exchanger, isBuys)+"/"+"ledger"
  def marginFile(year : Int, exchanger : Exchanger, isBuys : Boolean, id : String) = marginFolder(year, exchanger, isBuys)+"/"+id+stockExtension
  def marginLedgerFile(year : Int, exchanger : Exchanger, isBuys : Boolean, id : String) = marginLedgerFolder(year, exchanger, isBuys)+"/"+id+stockExtension


  def mkDir(path : String): Unit = {
    val folder = new java.io.File(path)
    if (!folder.exists())
      folder.mkdirs()
  }

  def getParent(path : String) : String =
    decompose(path)._1

  def getName(path : String) : String =
    decompose(path)._2

  def getExtension(path : String) : String =
    decompose(path)._3

  def decompose(path : String) : (String, String, String) = {
    val file = new File(path)
    val name = file.getName
    val idx = name.lastIndexOf(".")
    return(file.getParent, name.substring(0, idx), name.substring(idx))
  }

  def compose(paths : Seq[String], name : String, ext : String) : String = {
    val ext1 = if(ext.head=='.') ext else "."+ext
    return paths.mkString("/")+"/"+name+ext1
  }

  override def init(): Unit = {
    // for(folderName <- List(userInputFolder, userOutputFolder, userCacheFolder, userHistoryFolder))
    //  mkDir(folderName)
  }

  object PrintStream {
    def apply(file : File): java.io.PrintStream = {
      val path = file.getParentFile

      if(!path.exists())
        path.mkdirs()

      new java.io.PrintStream(file)
    }

    def apply(fileName : String): java.io.PrintStream =
      apply(new File(fileName))
  }

  def withPrintStream(file : java.io.File)(p : java.io.PrintStream => Unit): Unit = {
    var ps : java.io.PrintStream = null
    try {
      ps = PrintStream(file)
      p(ps)
    } finally {
      if (ps != null)
        ps.close()
    }
  }

  def withPrintStream(fileName : String)(p : java.io.PrintStream => Unit): Unit = {
    withPrintStream(new File(fileName))(p)
  }

  def withSource[A](file : java.io.File)(p : scala.io.Source => A) : A = {
    var src : scala.io.Source = null
    try {
      src = scala.io.Source.fromFile(file)
      val x = p(src)
      return x
    } finally {
      if(src != null)
        src.close()
    }
  }

  def withSource[A](fileName : String)(p : scala.io.Source => A) : A =
    withSource(new File(fileName))(p)
}
