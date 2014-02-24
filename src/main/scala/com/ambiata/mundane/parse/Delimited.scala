package com.ambiata.mundane.parse

object Delimited {

  def parsePsv(str: String) =
    parseRow(str, '|')

  def parseCsv(str: String) =
    parseRow(str, ',')

  def parseTsv(str: String) =
    parseRow(str, '\t')

  /**
   * imperative parsing of a csv line allowing to escape field with quotes
   */
  def parseRow(row: String, delimiter: Char): Seq[String] = {
    // an empty row must just be returned as an empty field
    if (row.isEmpty) Seq("")
    else {
      val field = new scala.collection.mutable.ListBuffer[Char]
      val fields = new scala.collection.mutable.ListBuffer[String]
      // we need a bit of look-ahead for single quotes in quoted fields
      // because they can be escaped with another quote: "this is, a "" quoted, field" -> List("this is", " a \" quoted", " field")
      val line = (row zip (row.tail :+ "\n")).iterator
      // are we quoting a field or not?
      var quoted = false

      while(line.hasNext) {
        val (character, nextCharacter) = line.next

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
        } else {
          // if we find a quote
          if (character == '"') {
            // it might be a protected quote inside a field
            if (nextCharacter == '"' && quoted) {
              field.append(character)
              quoted = !quoted // reverse the action that's going to happen with the next character...
            }
            // otherwise toggle the quoted state
            else quoted = !quoted
          }
          // otherwise, if the character is not a delimiter or a quote, simply add it
          else field.append(character)
        }
      }

      fields.append(field.mkString(""))
      fields.toList
    }
  }

}
