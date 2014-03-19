package com.ambiata.mundane
package testing

import com.ambiata.mundane.control._
import org.specs2._, matcher._, execute.{Result => SpecsResult, Error => SpecsError, _}
import scalaz.effect.IO

object ResultTMatcher extends ThrownExpectations {
  def beOk[A]: Matcher[ResultT[IO, A]] =
    beOkLike(_ => Success())

  def beOkValue[A](expected: A): Matcher[ResultT[IO, A]] =
    beOkLike((actual: A) => new BeEqualTo(expected).apply(createExpectable(actual)).toResult)

  def beOkLike[A](check: A => SpecsResult): Matcher[ResultT[IO, A]] = new Matcher[ResultT[IO, A]] {
    def apply[S <: ResultT[IO, A]](attempt: Expectable[S]) = {
      val r = attempt.value.run.unsafePerformIO match {
        case Ok(actual)     => check(actual)
        case Error(error)   => Failure(s"Result failed with <${Result.asString(error)}>")
      }
      result(r.isSuccess, r.message, r.message, attempt)
    }
  }
}
