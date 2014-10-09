package com.ambiata.mundane.reflect

import org.specs2._

class ToStringSpec extends Specification { def is = s2"""

 A case class "toString" method can be implemented with a macro
 so that field names are displayed before the corresponding value ${
    Point(1, 2).toString ===
    """|Point(
       |  x = 1
       |  y = 2
       |)""".stripMargin
  }

"""

  case class Point(x: Int, y: Int) {
    override def toString: String =
      macro ToString.toStringWithNames
  }
}
