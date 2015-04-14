package com.ambiata.mundane
package csv

import scalaz.\/

trait CsvParser {
  def parse(line: String): String \/ List[String]
}
