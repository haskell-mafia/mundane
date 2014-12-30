package com.ambiata.mundane.testing

import org.scalacheck._
import Arbitrary._
import scalaz._, Scalaz._, effect.IO
import com.ambiata.mundane.control._

object Arbitraries {
  implicit def ResultArbitrary[A: Arbitrary]: Arbitrary[Result[A]] =
    Arbitrary(arbitrary[(String \&/ Throwable) \/ A].map(Result.fromDisjunction))

  implicit def ResultTArbitrary[F[+_], A](implicit F: Functor[F], A: Arbitrary[F[(String \&/ Throwable) \/ A]]): Arbitrary[ResultT[F, A]] = {
    Functor[F]
    Arbitrary(arbitrary[F[(String \&/ Throwable) \/ A]].map(ResultT.fromDisjunctionF[F, A]))
  }

  implicit def RIOArbitrary[A](implicit A: Arbitrary[(String \&/ Throwable) \/ A]): Arbitrary[RIO[A]] = {
    Arbitrary(arbitrary[(String \&/ Throwable) \/ A].map(RIO.fromDisjunction[A](_)))
  }

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
