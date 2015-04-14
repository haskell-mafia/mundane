package com.ambiata.mundane
package csv

import scala.collection.JavaConversions._
import scalaz._, Scalaz._

/**
 * Implementation of a CsvParser based on the simple csv library
 */
case class BasicCsv(delimiter: Char, quoteCharacter: Option[Char], escapeCharacter: Option[Char]) {
  def parse(line: String): String \/ List[String] = {
    val quote = quoteCharacter.getOrElse('"')
    val escape = escapeCharacter.getOrElse('"')

    // an empty line must just be returned as an empty field
    if (line.isEmpty) \/-(List(""))
    else try {
      val field = new scala.collection.mutable.ListBuffer[Char]
      val fields = new scala.collection.mutable.ListBuffer[String]
      // we need a bit of look-ahead for single quotes in quoted fields
      // because they can be escaped with another quote: "this is, a "" quoted, field" -> List("this is", " a \" quoted", " field")
      val lineOption = line.map(Option.apply)
      val record = (lineOption zip (lineOption.tail:+ None) zip (None +: lineOption)).iterator
      // are we quoting a field or not?
      var quoted = false
      var escaped = false

      while (record.hasNext) {
        val ((Some(character), nextCharacter), previousCharacter) = record.next

        if (character == delimiter) {
          // if we are quoting the field, a delimiter is part of the field as
          // any other character
          if (quoted) field.append(character)
          // otherwise a delimiter marks the end of a field
          // we must start a new one
          else {
            fields.append(field.mkString(""))
            field.clear
          }
        } else if (quoted && character == escape && !escaped && nextCharacter.contains(quote)) {
           escaped = true
        }
        else if (character == quote) {
          if (quoted) {
            if ((nextCharacter.contains(delimiter) || !nextCharacter.isDefined) && !escaped) quoted = false
            else if (!escaped)
              throw BasicCsvParseException("A quote must be escaped inside a quoted field")
            else {
              field.append(character)
              escaped = false
            }
          } else {
            if (!previousCharacter.isDefined || previousCharacter.contains(delimiter)) quoted = true
            else if (!escaped)
              throw BasicCsvParseException("A quote must be escaped inside a quoted field")
            // actually closing quote for an empty field
            else if (field.isEmpty && nextCharacter.contains(delimiter)) escaped = false
            else {
              field.append(character)
              escaped = false
            }
          }
        }
        // otherwise, if the character is not a delimiter or a quote, simply add it
        else field.append(character)
      }

      fields.append(field.mkString(""))
      \/-(fields.toList)
    } catch {
      case BasicCsvParseException(m) => -\/(m)
    }
  }
}

case class BasicCsvParseException(m: String) extends Exception

object BasicCsv {
  def delimited(delimiter: Char): BasicCsv =
    BasicCsv(delimiter, '"'.some, none[Char])

  def csv: BasicCsv =
    delimited(',')

  def psv: BasicCsv =
    delimited('|')

  def tsv: BasicCsv =
    delimited('\t')

}
