package taxes

object AddressBook extends Initializable {

  private val folderSource = new FolderSource[(String,String)](io.FileSystem.addressBookFolder, ".txt") {
    def fileSource(fileName: String) = new FileSource[(String, String)](fileName) {
      override def read(): Seq[(String, String)] =
        util.parse.Parse.readKeysValue(fileName, s"Reading address book").toSeq
    }
  }

  private val book: Map[String,String] = // from address to description
    folderSource.read().toMap

  def richElem(currency: Currency, address: String): RichText.Elem = book.get(address) match {
    case None =>
      s"${RichText.address(currency, address)}"
    case Some(desc) =>
      s"${RichText.address(currency, address)} ($desc)"
  }
}
