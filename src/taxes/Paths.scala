package taxes

import taxes.Market.Market

object Paths extends Initializable {
  val data = "data"
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

  lazy val cacheFolder = usr+"/"+Config.config.user+"/cache"

  override def init(): Unit = {
    for(folderName <- List(userInputFolder, cacheFolder)) {
      val folder = new java.io.File(folderName)
      if (!folder.exists())
        folder.mkdir()
    }
  }
}
