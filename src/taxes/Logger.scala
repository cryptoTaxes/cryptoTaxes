package taxes

object Logger extends Initializable with Finalizable {
  private lazy val warningStream = new java.io.PrintStream(Paths.userOutputFolder+"/warnings.txt")

  def trace(what : String): Unit = {
    println(what)
  }

  def fatal(what : String): Nothing = {
    sys.error(what)
  }

  def warning(what : String): Unit = {
    warningStream.println("Warning: " + what)
  }

  override def wrapUp(): Unit = {
    warningStream.close()
  }
}
