package com.ambiata.mundane
package csv

import scalaz._, Scalaz._

/**
 * Implementation of a CsvParser based on the Parboiled library
 */
case class ParboiledCsv(delimiter: Char, quoteCharacter: Option[Char], escapeCharacter: Option[Char]) extends CsvParser {
  def parse(line: String): String \/ List[String] =
    \/.fromEither(
      ParboiledParser(line, delimiter, quoteCharacter.getOrElse('"'), escapeCharacter.getOrElse('"')).parseRow).leftMap(_.formatExpectedAsString)
}

object ParboiledCsv {
  def delimited(delimiter: Char): ParboiledCsv =
    ParboiledCsv(delimiter, '"'.some, none[Char])

  def csv: ParboiledCsv =
    delimited(',')

  def psv: ParboiledCsv =
    delimited('|')

  def tsv: ParboiledCsv =
    delimited('\t')
}



import org.parboiled2._
import org.parboiled2.Parser
import Parser.DeliveryScheme.Either

case class ParboiledParser(input: ParserInput, DELIMITER: Char, QUOTE: Char, ESCAPE: Char, whiteSpace: String = " \t") extends Parser {

  val WHITESPACE = CharPredicate(whiteSpace)

  def DELIMITER_TOKEN = rule(capture(DELIMITER))
  def DQUOTE2         = rule(s"$ESCAPE$QUOTE" ~ push(s"$QUOTE"))  // combine 2 quotes into 1
  def TXT             = rule(capture(!anyOf(DELIMITER.toString+QUOTE) ~ ANY))
  def SPACES          = rule(oneOrMore(WHITESPACE))

  def escaped    = rule(optional(SPACES) ~ QUOTE ~ (zeroOrMore(DELIMITER_TOKEN|TXT|DQUOTE2) ~ QUOTE ~ optional(SPACES)) ~> (_.mkString("")))
  def nonEscaped = rule(zeroOrMore(TXT) ~> (_.mkString("")))
  def field      = rule(escaped | nonEscaped)
  def row        = rule(zeroOrMore(field).separatedBy(DELIMITER) ~ EOI)

  def parseRow: ParseError Either List[String] = row.run().map(_.toList)
}

