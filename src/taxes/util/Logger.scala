package taxes.util

import taxes.{Finalizable, Initializable, FileSystem}


object Logger extends Initializable with Finalizable {
  private lazy val warningStream = FileSystem.PrintStream(FileSystem.userOutputFolder+"/warnings.txt")
  private lazy val traceStream = FileSystem.PrintStream(FileSystem.userOutputFolder+"/trace.txt")

  def trace(what : String): Unit = {
    traceStream.println(what)
    println(what)
  }

  def fatal(what : String): Nothing = {
    traceStream.println("FATAL ERROR: "+what)
    sys.error(what)
  }

  def warning(what : String): Unit = {
    warningStream.println("Warning: " + what)
  }

  override def wrapUp(): Unit = {
    warningStream.close()
    traceStream.close()
  }
}
