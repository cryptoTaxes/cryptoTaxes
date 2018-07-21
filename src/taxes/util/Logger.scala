package taxes.util

import taxes.io.FileSystem
import taxes.{Finalizable, Initializable}


object Logger extends Initializable with Finalizable {
  private lazy val warningStream = FileSystem.PrintStream(FileSystem.File(s"${FileSystem.userOutputFolder}/warnings.txt"), doBackUp = false)
  private lazy val traceStream = FileSystem.PrintStream(FileSystem.File(s"${FileSystem.userOutputFolder}/trace.txt"), doBackUp = false)

  def trace(what : String): Unit = {
    traceStream.println(what)
    println(what)
  }

  def fatal(what : String): Nothing = {
    traceStream.println(s"FATAL ERROR: $what")
    sys.error(what)
  }

  def warning(what : String): Unit = {
    warningStream.println(s"Warning: $what")
  }

  override def wrapUp(): Unit = {
    warningStream.close()
    traceStream.close()
  }
}
