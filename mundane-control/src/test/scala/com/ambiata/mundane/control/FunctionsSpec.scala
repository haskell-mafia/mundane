package com.ambiata.mundane
package control

import org.specs2._

class FunctionsSpec extends Specification with ScalaCheck with Functions { def is = s2"""

Functions should be composeable with:
  disjunction                                   $prop_or
  conjunction                                   $prop_and
  negation                                      $prop_not

"""

  def prop_or = prop((f: Int => Boolean, g: Int => Boolean, v: Int) => {
    val actual = (f || g)(v)
    val expected = f(v) || g(v)
    expected === actual
  })

  def prop_and = prop((f: Int => Boolean, g: Int => Boolean, v: Int) => {
    val actual = (f && g)(v)
    val expected = f(v) && g(v)
    expected === actual
  })

  def prop_not = prop((f: Int => Boolean, v: Int) => {
    val actual = (! f)(v)
    val expected = !f(v)
    expected === actual
  })
}
