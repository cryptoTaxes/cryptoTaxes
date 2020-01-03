package taxes

object AddressBook extends Initializable {
  private val book: Map[String,String] = // from address to description
    util.parse.Parse.readKeysValue(io.FileSystem.addressBookFile, "Reading address book")

  def richElem(currency: Currency, address: String): RichText.Elem = book.get(address) match {
    case None =>
      s"${RichText.address(currency, address)}"
    case Some(desc) =>
      s"${RichText.address(currency, address)} ($desc)"
  }
}
