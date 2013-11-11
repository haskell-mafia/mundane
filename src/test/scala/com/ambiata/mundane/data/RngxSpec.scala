package com.ambiata.mundane
package data

import org.specs2.{ScalaCheck, Specification}
import scalaz._, Scalaz._
import NonEmptyList._
import Rngx._
import com.nicta.rng.{Size, Rng}
import Rng._
import org.specs2.matcher.Matcher

class RngxSpec extends Specification { def is = s2"""

  We can filter an existing generator with a predicate
  ${ int.list(10).filter(isEven).runIO must contain(isEven).forall }
  ${ int.list(10).filterNot(isOdd).runIO must contain(isEven).forall }

  We can generate an infinite stream of values from a generator
  ${ chooseint(1, 3).infinite.runIO.take(4).toList must contain(between(1, 3)).forall }

  We can pick a subset of a given list of values
  ${ List(1, 2, 2, 3).subset(3).runIO must contain(beOneOf(1, 2, 2, 3)).forall }
  ${ List(1, 2, 2, 3).subset(3).runIO must haveSize(3) }
  ${ List(1, 2, 2, 3).subset(10).runIO must haveSize(4) }
  ${ nel(0, List(1, 2, 2, 3)).subset1(3).runIO.list must haveSize(3) }
  ${ nel(0, List(1, 2, 2, 3)).subset1(Size(3)).runIO.size must be_<=(3) }

  We can generate lists of having a size specified by another generator
  ${ chooseint(1, 10).list(chooseint(1, 3)).runIO.size must beBetween(1, 3) }

"""

  def beDistinct[T]: Matcher[Seq[T]] = (seq: Seq[T]) => (seq.distinct.size == seq.size, s"$seq doesn't have distinct elements")
  def isEven = (n: Int) => n % 2 == 0
  def isOdd  = (n: Int) => !isEven(n)
}
