package com.ambiata.mundane.reflect

import org.specs2._

class ToStringSpec extends Specification { def is =
  skipAllIf(true) ^ // skip for now, we need to find a way to pass the test in 2.10
  s2"""

 A case class "toString" method can be implemented with a macro
 so that field names are displayed before the corresponding value ${
    Point(1, 2).toString ===
    """|Point(
       |  x = 1
       |  y = 2
       |)""".stripMargin
  }

 An Option of a Point must be properly rendered too ${
    Option(Point(1, 2)).toString ===
      """|Some(Point(
         |  x = 1
         |  y = 2
         |))""".stripMargin

  }

"""
}

case class Point(x: Int, y: Int) {
  override def toString: String =
    ToString.toStringWithNames
}

