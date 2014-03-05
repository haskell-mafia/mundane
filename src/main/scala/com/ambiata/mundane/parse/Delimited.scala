package com.ambiata.mundane.parse

object Delimited {

  def parsePsv(str: String) =
    DelimitedParser2(str, "|").parseRow

  def parseCsv(str: String) =
    DelimitedParser2(str, ",").parseRow

  def parseTsv(str: String) =
    DelimitedParser2(str, "\t", " ").parseRow

  def parseRow(str: String, delimiter: Char) =
    DelimitedParser2(str, delimiter.toString).parseRow

}

import org.parboiled2._
import org.parboiled2.Parser

case class DelimitedParser2(input: ParserInput, DELIMITER: String, whiteSpace: String = " \t") extends Parser {

  val DQUOTE = '"'
  val DELIMITERS = s""""$DELIMITER\r\n"""
  val WHITESPACE = CharPredicate(whiteSpace)

  def DELIMITER_TOKEN = rule(capture(DELIMITER))
  def DQUOTE2         = rule("\"\"" ~ push("\""))  // combine 2 dquotes into 1
  def CRLF            = rule(capture("\r\n" | "\n"))
  def TXT             = rule(capture(!anyOf(DELIMITERS) ~ ANY))
  def SPACES          = rule(oneOrMore(WHITESPACE))

  def escaped    = rule(optional(SPACES) ~ DQUOTE ~ (zeroOrMore(DELIMITER_TOKEN|TXT|CRLF|DQUOTE2) ~ DQUOTE ~ optional(SPACES)) ~> (_.mkString("")))
  def nonEscaped = rule(zeroOrMore(TXT) ~> (_.mkString("")))
  def field      = rule(escaped | nonEscaped)
  def row        = rule(zeroOrMore(field).separatedBy(DELIMITER))

  def parseRow = row.run().toOption.getOrElse(Nil)
}

