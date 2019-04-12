package taxes.util.parse

import taxes.io.FileSystem

import org.apache.poi.ss.usermodel.{DataFormatter, Row, WorkbookFactory}
import scala.collection.mutable.ListBuffer

object ExcelReader {
  def XLSXToCSV(xlsxFileName: String, csvFileName: String, sheetIndex: Int = 0, sep: Char = ','): Unit = {
    val workbook = WorkbookFactory.create(FileSystem.File(xlsxFileName))
    val sheet = workbook.getSheetAt(sheetIndex)
    val formatter = new DataFormatter()

    FileSystem.withPrintStream(csvFileName, doBackUp = false) { ps =>
      for (r <- sheet.getFirstRowNum to sheet.getLastRowNum) {
        val row = sheet.getRow(r)
        val strings = ListBuffer[String]()

        for (col <- row.getFirstCellNum until row.getLastCellNum) {
          val cell = row.getCell(col, Row.MissingCellPolicy.RETURN_BLANK_AS_NULL)
          val str = if (cell == null) "" else formatter.formatCellValue(cell)
          strings.append(str)
        }
        ps.println(strings.mkString(sep.toString))
      }
    }
  }
}
