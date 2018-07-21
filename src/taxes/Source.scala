package taxes

import taxes.io.FileSystem
import taxes.util.Logger

import scala.collection.mutable.ListBuffer


trait Source[+A] {
  def read() : Seq[A]
}

abstract case class FileSource[+A](fileName : String) extends Source[A]

abstract class FolderSource[+A](folderPath : String, extension : String) extends Source[A] {
  def fileSource(fileName : String) : FileSource[A]

  def read(): Seq[A] = {
    val xs = ListBuffer[A]()
    val files = FileSystem.findFilesAt(folderPath, extension)
    if (files != null)
      for (file <- files) {
        val fileName = file.getPath
        Logger.trace(s"Reading contents of file $fileName.")
        xs ++= fileSource(fileName).read()
      }
    return xs
  }
}
