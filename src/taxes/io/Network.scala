package taxes.io

import java.net.{HttpURLConnection, URL}
import java.nio.charset._

import taxes.util.Logger

object Network {
  object Http {
    private val userAgent = "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.11 (KHTML, like Gecko) Chrome/23.0.1271.95 Safari/537.11"

    case class Output(contentType: String, contents: String)

    def _withSource[A](url: String, requestMethod: String = "GET", charset: Charset = StandardCharsets.UTF_8, output: Output = null)(p : scala.io.Source => A) : A = {
      var conn: HttpURLConnection = null
      var src: scala.io.Source = null

      try {
        Logger.trace(s"Downloading from $url.")
        conn = new URL(url).openConnection.asInstanceOf[HttpURLConnection]
        conn.addRequestProperty("User-Agent", userAgent)
        conn.setRequestMethod(requestMethod)

        if(output==null)
          conn.connect()
        else {
          conn.setDoOutput(true)
          val bytes = output.contents.getBytes(charset)

          conn.setFixedLengthStreamingMode(bytes.length)
          conn.setRequestProperty("Content-Type", s"${output.contentType}; charset=$charset")
          conn.connect()

          val os = conn.getOutputStream
          try
            os.write(bytes)
          finally
            if (os != null)
              os.close()
        }
        val is = conn.getInputStream
        val codec = charset.name
        src = scala.io.Source.fromInputStream(is, codec)
        val x = p(src)
        return x
      } finally {
          // toDo if src escapes p procedure scope, we will close connection before reading from it
         if (conn != null)
          conn.disconnect()
        if(src != null)
          src.close()
      }
    }

    def withSource[A](url: String, requestMethod: String, charset: Charset, output: Output)(p : scala.io.Source => A) : A =
      _withSource(url, requestMethod, charset, output)(p)

    def withSource[A](url: String, charset: Charset, output: Output)(p : scala.io.Source => A) : A =
      _withSource(url, charset = charset, output = output)(p)

    def withSource[A](url: String, requestMethod: String, output: Output)(p : scala.io.Source => A) : A =
      _withSource(url, requestMethod, output = output)(p)

    def withSource[A](url: String, requestMethod: String, charset: Charset)(p : scala.io.Source => A) : A =
      _withSource(url, requestMethod, charset)(p)

    def withSource[A](url: String, output: Output)(p : scala.io.Source => A) : A =
      _withSource(url, output = output)(p)

    def withSource[A](url: String, charset: Charset)(p : scala.io.Source => A) : A =
      _withSource(url, charset = charset)(p)

    def withSource[A](url: String, requestMethod: String)(p : scala.io.Source => A) : A =
      _withSource(url, requestMethod = requestMethod)(p)

    def withSource[A](url: String)(p : scala.io.Source => A) : A =
      _withSource(url)(p)


    object Json {
      import spray.json._
      import spray.json.JsonProtocol._

      case class Response(jsonrpc: String, id: Int, error: Option[JsObject], result: Option[JsObject])
      implicit val response = jsonFormat4(Response)

      private def _RPC(url: String, charset: Charset = defaultCharset, command: JsObject): JsObject = {
        val output = Network.Http.Output(contentType = "application/json", contents = command.compactPrint)
        Network.Http.withSource(url, requestMethod = "POST", charset = charset, output) { src =>
          val str = src.mkString
          val response = spray.json.JsonParser(str).asJsObject.convertTo[Response]
          response.error match {
            case Some(err) =>
              Logger.fatal(s"There was an error in response for json RPC call $command\nError was $err")
            case None =>
              response.result match {
                case None => null
                case Some(jsObject) => jsObject
              }
          }
        }
      }

      def RPC(url: String, charset: Charset, command: JsObject): JsObject =
        _RPC(url, charset, command)

      def RPC(url: String, command: JsObject): JsObject =
        _RPC(url, command = command)
    }
  }
}
