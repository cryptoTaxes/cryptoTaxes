package taxes.io

import taxes.util.Logger

object Network {
  def fromHttpURL(urlString : String) : String = {
    import java.io.BufferedInputStream
    import java.net.{HttpURLConnection, URL}

    Logger.trace(s"Downloading from $urlString")

    val sb = new StringBuilder
    var conn : HttpURLConnection = null
    var in : BufferedInputStream = null

    try {
      val url = new URL(urlString)
      conn = url.openConnection.asInstanceOf[HttpURLConnection]
      conn.addRequestProperty("User-Agent", "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.11 (KHTML, like Gecko) Chrome/23.0.1271.95 Safari/537.11")
      conn.connect()
      in = new BufferedInputStream(conn.getInputStream)
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
}


object test extends App {
/*
  val url = "https://www.bde.es/webbde/es/estadis/infoest/series/tc_1_1.csv"
  val str = Network.fromHttpURL(url)
  FileSystem.withPrintStream("./down.csv"){ ps =>
    ps.print(str)
  }
*/
  val url = "https://www.bde.es/webbde/es/estadis/infoest/series/tc_1_1.csv"
  val source = scala.io.Source.fromURL(url)(scala.io.Codec.ISO8859)
  val fileName = "./down2.csv"
  FileSystem.withPrintStream(fileName) { ps =>
    ps.print(source.mkString)
  }
}