package com.ambiata.mundane
package data

import org.specs2._
import org.scalacheck._, Arbitrary._


class ListsSpec extends Specification with ScalaCheck { def is = s2"""

Lists Properties
----------------

  There should be exactly 1 new line for each element      $newlines

"""

  def newlines =
    prop((n: List[Int]) =>
      Lists.prepareForFile(n.map(_.toString)).filter(_ == '\n').length must_== n.size)
}
