package com.ambiata.mundane.parse

object Delimited {

  def parsePsv(str: String) =
    DelimitedParser2(str, "|").row.run()

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
import org.parboiled2._

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


case class DelimitedParser2(input: ParserInput, DELIMITER: String) extends Parser {

  def DQUOTE: Rule0  = rule("\"")
  def DELIMITER_TOKEN: Rule1[String] = rule(capture(DELIMITER))
  def DQUOTE2: Rule1[String] = rule(capture("\"\"") ~> ((s: String) => "\""))  // combine 2 dquotes into 1
  def CRLF: Rule1[String]    = rule(capture("\r\n" | "\n"))
  def TXT: Rule1[String]     = rule(capture(!anyOf(s""""$DELIMITER\r\n""")))
  def SPACES: Rule0          = rule(oneOrMore(anyOf(" \t")))

  def escaped: Rule1[String] = rule(optional(SPACES) ~ DQUOTE ~ (zeroOrMore(DELIMITER_TOKEN|TXT|CRLF|DQUOTE2) ~ DQUOTE ~ optional(SPACES)) ~> ((_:Seq[_]).mkString("")))
  def nonEscaped: Rule1[String] = rule(zeroOrMore(TXT) ~> ((t: Seq[String]) => t.mkString("")))
  def field = rule(escaped | nonEscaped)
  def row: Rule1[Seq[String]] = rule(oneOrMore(field).separatedBy(DELIMITER))
//
//  def parseRow(str: String): List[String] = super.parse(row, str).getOrElse(Nil)
}