package taxes.io

import java.net.{HttpURLConnection, URL}
import java.nio.charset._

import taxes.util.Logger

object Network {
  object Http {
    private val userAgentKey = "User-Agent"
    private val userAgentVal = "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.11 (KHTML, like Gecko) Chrome/23.0.1271.95 Safari/537.11"

    case class Error(code : Int) extends Exception {
      override def toString: String = s"Network.Http.Error($code)"
    }

    trait Output

    object Output {
      case object None extends Output
      case class String(string: scala.Predef.String) extends Output
      case class Bytes(bytes : Array[Byte]) extends Output
    }

    def withSource[A](url: String, requestMethod: String = "GET", charset: Charset = StandardCharsets.UTF_8, requestProperties : Map[String,String] = Map(), output: Output = Output.None)(p : scala.io.Source => A) : A = {
      var conn: HttpURLConnection = null
      var src: scala.io.Source = null

      try {
        Logger.trace(s"Downloading from $url.")
        conn = new URL(url).openConnection.asInstanceOf[HttpURLConnection]

        conn.addRequestProperty(userAgentKey, userAgentVal)
        for((key, value) <- requestProperties)
          conn.addRequestProperty(key, value)

        conn.setRequestMethod(requestMethod)
        conn.setDoInput(true)

        output match {
          case Output.None =>
            conn.connect()
          case _ =>
            conn.setDoOutput(true)

            val bytes = output match {
              case Output.Bytes(bytes) =>
                bytes
              case Output.String(string) =>
                string.getBytes(charset)
            }

            conn.setFixedLengthStreamingMode(bytes.length)
            conn.connect()

            val os = conn.getOutputStream
            try
              os.write(bytes)
            finally
              if (os != null)
                os.close()
        }

        val responseCode = conn.getResponseCode
        if(responseCode != 200)
          throw Error(responseCode)

        val is = conn.getInputStream
        src = scala.io.Source.fromInputStream(is, charset.name)
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

    object Json {
      import spray.json._
      import spray.json.JsonProtocol._

      case class Error(code : Int, message : String, data : Option[JsValue] = None) extends Exception {
        override def toString = s"Http.Json.Error($code, $message, $data)"
      }
      implicit val error = jsonFormat3(Error)

      case class Request(jsonrpc: String, id : Int, method : String, params : Option[JsValue])
      implicit val request = jsonFormat4(Request)

      case class Response(jsonrpc: String, id : Int, error: Option[Error], result: Option[JsObject])
      implicit val response = jsonFormat4(Response)

      private var id = 0

      private val jsonrpc = "2.0"

      def RPC(url: String, charset: Charset = defaultCharset, method: String, params : JsValue = null): JsObject = {
        id += 1
        val request =
          Request(
              jsonrpc = jsonrpc
            , id = id
            , method = method
            , params = params match {
                case null =>
                  None
                case jsObject : JsObject =>
                  Some(jsObject)
                case jsArray : JsArray =>
                  Some(jsArray)
                case _ =>
                  throw Error(-32602, s"Network.Http.Json._RPC: params should be a JSON array or object.\nparams 0 $params.")
            }
          )

        val bytes = request.toJson.compactPrint.getBytes(charset)

        val requestProperties =
          Map( "Content-Type" -> "application/json"
             , "charset" -> charset.name()
             , "Content-Length" -> bytes.length.toString
             , "Accept" -> "application/json"
             )

       val output = Network.Http.Output.Bytes(bytes = bytes)

       Network.Http.withSource(url = url, requestMethod = "POST", charset = charset, requestProperties = requestProperties, output = output){ src =>
         val response = src.mkString.parseJson.convertTo[Response]

         if(response.id != id)
           throw Error(-32099, s"Response id(${response.id}) does not match request id(${request.id}).")
         else
           response.error match {
             case Some(err) =>
               throw err
             case None =>
               response.result match {
                 case None => null
                 case Some(jsObject) => jsObject
               }
            }
        }
      }
    }
  }
}
