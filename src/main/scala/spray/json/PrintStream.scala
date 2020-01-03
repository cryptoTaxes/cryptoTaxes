package spray.json

import java.io.PrintStream

object PrintStream {
  def prettyPrintOn[T](ps : PrintStream, iterable: Iterable[T])(implicit writer: JsonWriter[T]): Unit = {
    var i = iterable.size
    if(i > 0) {
      ps.print("[")
      for(elem <- iterable) {
        i -= 1
        ps.print(elem.toJson.prettyPrint)
        ps.println(if (i > 0) "," else "]")
      }
    }
  }
}
