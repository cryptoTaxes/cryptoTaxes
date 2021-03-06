package taxes


import taxes.io.FileSystem
import taxes.util.Logger

import scala.collection.mutable.ListBuffer


trait ToHTML {
  def toHTML: HTML
}


object HTMLDoc {

  def box(header: Any, boxBody: Any): HTML =
    <div class='boxed'>
      <div class='boxHeader'>
          {header}
      </div>
      <div class='boxBody'>
        {boxBody}
      </div>
    </div>


  def header4(header0: Any, header1: Any, header2: Any, header3: Any): HTML =
    <span>
      <span class='header0'>
        {header0}
      </span>
      <span class='header1'>
        {header1}
      </span>
      <span class='header2'>
        {header2}
      </span>
      <span class='header3'>
        {header3}
      </span>
    </span>


  trait Boxed {
    def headerToHTML: HTML

    def bodyToHTML: HTML

    def toHTML: HTML =
      box(headerToHTML, bodyToHTML)
  }
}


case class HTMLDoc(fileName: String, title: String) {
  private val allHTMLs = ListBuffer[ToHTML]()

  def +=(html: ToHTML): Unit =
    allHTMLs += html

  def +=(html: HTML): Unit = {
    allHTMLs += new ToHTML {
      override def toHTML: HTML = html
    }
  }

  private case class State(fileName: String, title: String)

  private var optState: Option[State] = Some(State(fileName, title))

  def setOutputTo(fileName: String): Unit = {
    val newState = State(fileName, title)

    optState match {
      case None     => ;
      case Some(st) =>
        if(allHTMLs.nonEmpty) {
          val ps = FileSystem.PrintStream(st.fileName)
          printlnPage(ps)
          ps.close()
        }
    }
    optState = Some(newState)
  }

  def close(): Unit = {
    optState match {
      case None     =>
        Logger.fatal("HTML.close: stream is already closed")
      case Some(st) =>
        if(allHTMLs.nonEmpty) {
          val ps = FileSystem.PrintStream(st.fileName)
          printlnPage(ps)
          ps.close()
        }
    }
    optState = None
  }

  private val styles =
    """
      |  body {font-family: 'Open Sans', sans-serif; -webkit-print-color-adjust: exact; }
      | .back1, .boxHeader, table#tableStyle1 caption, table#tableStyle1 tr:last-child {  background-color: #e0e0e0; }
      | .barL { border-left: 1px solid blue; }
      | .boxPadding, .boxBody, .boxHeader, table#tableStyle1 caption, table#tableStyle1 td, th { padding-top: 0.2em; padding-bottom: 0.2em; padding-left: 0.2em; padding-right: 0.2em; }
      |
      | .header { font-size: 130%; margin-bottom: 20px; font-weight: bold; }
      | .subheader { font-size: 110%; margin-bottom: 20px; font-weight: bold; }
      | .boxed { width: 98%; background-color: White;  border: 1px solid black; border-collapse: collapse; margin-bottom: 1em; page-break-inside:avoid; page-break-after:auto; }
      | .boxHeader {  border-bottom: 1px solid black; border-collapse: collapse; }
      | .header0 { width: 7%; display: inline-block; }
      | .header1 { width: 15%; display: inline-block; }
      | .header2 { width: 65%; display: inline-block; }
      | .header3 { width: 10%; display: inline-block; text-align: right; }
      | .currency { color: blue; }
      | .addr { color: #0000b0; text-decoration: none; ; font-size: 85%; }
      | .tx { color: green; text-decoration: none; font-size: 85%; }
      | .opNumber { color: #0000b0; text-decoration: none; }
      | .exchanger { color: green; }
      | .operationNumber { color: navy; }
      | .boxBody { background-color: White;}
      | .embold { font-weight: bold; font-size: 95%;}
      | .small1 { font-size: 85%; }
      | .small2, .desc, .rates, .stock, .footnote { font-size: 80%; }
      | .noLineBreak { white-space:nowrap; }
      | .marginBottom5, .desc, .rates { margin-bottom: 0.25em; }
      | .marginTopBottom20, .footnote { margin-top: 0.5em; margin-bottom: 0.5em; }
      | .paddingR10 { padding-right: 0.2em; }
      | .alignL { text-align: left; }
      | .alignR { text-align: right; }
      | .darkBlue { color: #0000b0; }
      | .darkRed { color: #b00000; }
      | .darkMagenta { color: #a000a0; }
      | .noDecor { color: black; text-decoration: none; }
      |
      | table#tableStyle1 { border: 1px solid black; border-collapse: collapse; margin-bottom: 20px; page-break-inside:avoid; }
      | table#tableStyle1 caption { border: 1px solid black; border-bottom: 0px solid black; font-weight: bold; text-align: left; }
      | table#tableStyle1 tr.caption { border: 1px solid black; border-bottom: 0px solid black; font-weight: bold; text-align: left; }
      | table#tableStyle1 tr:nth-child(odd) { background-color: #f0f0f0;  }
      | table#tableStyle1 tr:hover { background-color: #c5c5c5; }
      | table#tableStyle1 td, th { text-align: left; vertical-align: center; }
      | table#tableStyle1 td:first-child, th:first-child { text-align: left; }
      | table#tableStyle1 td.alignT, th.alignL { vertical-align: top; }
      | table#tableStyle1 td.alignL, th.alignL { text-align: left; }
      | table#tableStyle1 td.alignR, th.alignR { text-align: right; }
      | table#tableStyle1 td.caption { border: 1px solid black; border-bottom: 0px solid black; font-weight: bold; text-align: left; }
      | table#tableStyle1 td.padding, th.padding { padding-left: 5px; padding-right: 5px; }
      | table#tableStyle1 td.paddingL, th.paddingL { padding-left: 5px; }
      | table#tableStyle1 td.paddingR, th.paddingR { padding-right: 5px; }
      |
      | @media print {
      |  body{
      |    font-size: 65%;
      |    column-count: 2;
      |    -webkit-column-count: 2;
      |    -moz-column-count: 2;
      |  }
      | .boxed { width: 100%; margin-bottom: 0.25em; }
      | .boxHeader { font-size: 90%; margin-bottom: 0.2em; }
      | .marginBottom5, .desc, .rates { margin-bottom: 0.2em; }
      | .marginTopBottom20, .footnote { margin-top: 0.25em; margin-bottom: 0.3em; }
      | }
      |""".stripMargin

  private def page(): HTML =
    <html>
    <head>
      <meta charset="UTF-8"></meta>
      <link href="https://fonts.googleapis.com/css?family=Open+SansOpen+Sans:300,400,600" rel="stylesheet"></link>
      <title>{optState.get.title}</title>
      <style type="text/css">
        {styles}
      </style>
    </head>
    <body>
      {allHTMLs.map({_.toHTML})}
      <br></br>
      <div class='footnote'>
      This document was generated by <a href='https://github.com/cryptoTaxes/cryptoTaxes'>cryptoTaxes</a>.
      </div>
    </body>
    </html>

  private def printlnPage(ps: FileSystem.PrintStream): Unit = {
    ps.println("<!DOCTYPE html>")
    ps.println(page())
    allHTMLs.clear()
  }
}
