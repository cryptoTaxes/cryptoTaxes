package taxes

import taxes.Util.Logger

trait Source[+A] {
  def read() : Seq[A]
}

abstract case class FileSource[+A](fileName : String) extends Source[A]

abstract class FolderSource[+A](folderPath : String) extends Source[A] {
  def fileSource(fileName : String) : FileSource[A]

  def read(): Seq[A] = {
    var xs = List[A]()
    val files = Paths.findFilesAt(folderPath)
    if (files != null)
      for (file <- files) {
        val fileName = file.getAbsolutePath
        Logger.trace("Reading contents of file %s.".format(fileName))
        xs ++= fileSource(fileName).read()
      }
    return xs
  }
}
