package com.ambiata.mundane.data

import util.matching.Regex

/**
 * Utility methods for regular expressions
 */
trait Regexes {

  /**
   * extract regular expression groups from a StringContext
   */
  implicit class RegexContext(sc: StringContext) {
    def r = new Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"): _*)
  }
}
object Regexes extends Regexes

