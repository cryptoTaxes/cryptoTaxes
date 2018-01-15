package taxes

import taxes.Market.Market

object Paths extends Initializable {
  val data = "data"
  val prices = data+"/prices"

  val coinMarketCap = prices+"/coinmarketcap"

  val euroUSDFile = prices+"/euroUSD/tc_1_1.csv"

  def coinMarketCapFile(market : Market) =
    coinMarketCap+"/"+market+".txt"

  val usr = data+"/usr"
  lazy val userInputFolder = usr+"/"+Config.config.user+"/input"
  lazy val userOutputFolder = usr+"/"+Config.config.user+"/output"

  val conf = data+"/config"
  def configFile(fileName : String) = conf+"/"+fileName

  def findFilesAt(path : String, extension : String = ".csv") : Array[java.io.File] = {
    val f = new java.io.File(path)
    val matchingFiles = f.listFiles(new java.io.FilenameFilter() {
      def accept(dir: java.io.File, name: String): Boolean =
        !name.startsWith("readme") && name.endsWith(extension)
    })
    return matchingFiles
  }

  override def init(): Unit = {
    val outputFolder = new java.io.File(userOutputFolder)
    if(!outputFolder.exists())
      outputFolder.mkdir()
  }
}
