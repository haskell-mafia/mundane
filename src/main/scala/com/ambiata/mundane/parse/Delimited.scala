package com.ambiata.mundane.parse

object Delimited {

  def parsePsv(str: String) =
    pipeParser.parseRow(str)

  def parseCsv(str: String) =
    commaParser.parseRow(str)

  def parseTsv(str: String) =
    tabParser.parseRow(str)

  private val pipeParser = new DelimitedParser {
    lazy val DELIMITER = "|"
  }

  private val commaParser = new DelimitedParser {
    lazy val DELIMITER = ","
  }

  private val tabParser = new DelimitedParser {
    lazy val DELIMITER = "\t"
    override val whiteSpace = " ".r
  }

}

import scala.util.parsing.combinator.RegexParsers

/**
 * This parser can parse rows with different delimiters
 */
trait DelimitedParser extends RegexParsers {
  override val skipWhitespace = false
  override val whiteSpace = """[ \t]""".r

  def DELIMITER: String
  def DQUOTE  = "\""
  def DQUOTE2 = "\"\"" ^^ { case _ => "\"" }  // combine 2 dquotes into 1
  def CRLF    = "\r\n" | "\n"
  def TXT     = s"""[^\"$DELIMITER\r\n]""".r
  def SPACES  = "[ \t]+".r

  def field: Parser[String]            = escaped|nonEscaped
  def row: Parser[List[String]]        = repsep(field, DELIMITER)
  def escaped: Parser[String]          = ((SPACES?) ~> DQUOTE ~> ((TXT|DELIMITER|CRLF|DQUOTE2)*) <~ DQUOTE <~ (SPACES?)).map(_.mkString(""))
  def nonEscaped: Parser[String]       = (TXT*).map(_.mkString(""))

  def parseRow(str: String): List[String] = super.parse(row, str).getOrElse(Nil)
}


