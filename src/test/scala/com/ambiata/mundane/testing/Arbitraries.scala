package com.ambiata.mundane
package testing

import control._
import org.scalacheck._, Arbitrary._
import scalaz._, Scalaz._

object Arbitraries {
  implicit def AttemptArbitrary[A: Arbitrary]: Arbitrary[Attempt[A]] =
    Arbitrary(arbitrary[(String \&/ Throwable) \/ A].map(Attempt.apply))

  /** WARNING: can't use scalaz-scalacheck-binding because of specs/scalacheck/scalaz compatibility at the moment */
  implicit def TheseArbitrary[A: Arbitrary, B: Arbitrary]: Arbitrary[A \&/ B] =
    Arbitrary(Gen.oneOf(
      arbitrary[(A, B)].map({ case (a, b) => \&/.Both(a, b) }),
      arbitrary[A].map(\&/.This(_): A \&/ B),
      arbitrary[B].map(\&/.That(_): A \&/ B)
    ))

  implicit def DisjunctionArbitrary[A: Arbitrary, B: Arbitrary]: Arbitrary[A \/ B] =
    Arbitrary(Gen.oneOf(
      arbitrary[A].map(-\/(_)),
      arbitrary[B].map(\/-(_))
    ))
}

