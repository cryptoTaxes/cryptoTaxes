package taxes

import taxes.io.FileSystem
import taxes.util.Logger

import scala.collection.mutable.ListBuffer


trait Source[+A] {
  def preprocess(): Option[() => Unit] = None

  def read() : Seq[A]
}

abstract case class FileSource[+A](fileName : String) extends Source[A]

abstract class FolderSource[+A](folderPath : String, extension : String) extends Source[A] {
  def fileSource(fileName : String) : FileSource[A]

  private def doWithAllFiles(procedure : String => Unit) {
    val files = FileSystem.findFilesAt(folderPath, extension)
    if (files != null)
      for (file <- files) {
        val fileName = file.getPath
        procedure(fileName)
      }
  }

  override def preprocess() = Some { () =>
    doWithAllFiles(fileName =>
        fileSource(fileName).preprocess() match {
          case None =>
            ;
          case Some(procedure) =>
            Logger.trace(s"Preprocessing contents of file $fileName.")
            procedure()
        }
    )
  }

  def read(): Seq[A] = {
    val xs = ListBuffer[A]()
    doWithAllFiles(fileName => {
      Logger.trace(s"Reading contents of file $fileName.")
      xs ++= fileSource(fileName).read()
    })
    return xs
  }
}
