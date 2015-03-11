package com.ambiata.mundane
package testing

import com.ambiata.mundane.parse._
import org.specs2._

import scalaz._, Scalaz._

class TextEscapingSpec extends Specification with ScalaCheck { def is = s2"""

Escaping
--------

  Escape and parsing should be symmetric                     $escapeAndSplit
"""

  def escapeAndSplit = prop {(s: List[String], c: Char) => s != List("") ==> {
    TextEscaping.split(c, TextEscaping.mkString(c, s)) ==== s
  }}
}
