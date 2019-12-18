package taxes

object AddressBook extends Initializable {
  private val book: Map[String,String] = // from address to description
    util.parse.Parse.readKeysValue(io.FileSystem.addressBookFile, "Reading address book")

  def format(address: String): String = book.get(address) match {
    case None =>
      address
    case Some(desc) =>
      s"$address ($desc)"
  }
}
