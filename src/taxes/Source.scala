package taxes

import taxes.Util.Logger

import scala.collection.mutable.ListBuffer

trait Source[+A] {
  def read() : Seq[A]
}

abstract case class FileSource[+A](fileName : String) extends Source[A]

abstract class FolderSource[+A](folderPath : String, extension : String) extends Source[A] {
  def fileSource(fileName : String) : FileSource[A]

  def read(): Seq[A] = {
    val xs = ListBuffer[A]()
    val files = Paths.findFilesAt(folderPath, extension)
    if (files != null)
      for (file <- files) {
        val fileName = file.getAbsolutePath
        Logger.trace("Reading contents of file %s.".format(fileName))
        xs ++= fileSource(fileName).read()
      }
    return xs
  }
}
