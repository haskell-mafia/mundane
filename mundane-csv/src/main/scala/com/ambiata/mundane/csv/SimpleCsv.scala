package com.ambiata.mundane
package csv

import net.quux00.simplecsv._

import scala.collection.JavaConverters._
import scalaz._, Scalaz._

/**
 * Implementation of a CsvParser based on the simple csv library
 *
 * It is a lot around 5 times faster than BasicCsv and ParboiledCsv
 * but it doesn't catch unquoted double-quotes in quoted fields
 * like "a "good day"
 *
 */
case class SimpleCsv(delimiter: Char, quoteCharacter: Option[Char], escapeCharacter: Option[Char]) extends CsvParser {
  private[this] val parser: net.quux00.simplecsv.CsvParser = {
    val builder = new CsvParserBuilder
    (quoteCharacter, escapeCharacter) match {
      case (Some(q), Some(e)) => builder.separator(delimiter).quoteChar(q).escapeChar(e)
      case (Some(q), None)    => builder.separator(delimiter).quoteChar(q).escapeChar('\\')
      case (None,    Some(e)) => builder.separator(delimiter).quoteChar('"').escapeChar(e)
      case (None,    None)    => builder.separator(delimiter).quoteChar('"').escapeChar('\\')
    }
    builder.multiLine(true).threadSafe(true).supportRfc4180QuotedQuotes(true).build
  }

  def parse(line: String): String \/ List[String] =
    \/.fromTryCatchNonFatal {
      if (line.isEmpty) List("")
      else parser.parse(line).asScala.toList
    }.leftMap(_.getMessage)

  def setQuoteCharacter(quote: Char): SimpleCsv =
    copy(quoteCharacter = Some(quote))

  def setEscapeCharacter(escape: Char): SimpleCsv =
    copy(escapeCharacter = Some(escape))
}

object SimpleCsv {
  def delimited(delimiter: Char): SimpleCsv =
    SimpleCsv(delimiter, '"'.some, none[Char])

  def csv: SimpleCsv =
    delimited(',')

  def psv: SimpleCsv =
    delimited('|')

  def tsv: SimpleCsv =
    delimited('\t')

}

