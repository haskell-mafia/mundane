package com.ambiata
package mundane
package testing

import control._
import org.specs2._
import matcher._
import execute.{Result => SpecsResult, Error => SpecsError, _}

object ResultMatcher extends ThrownExpectations {
  def beOk[A]: Matcher[Result[A]] =
    beOkLike(_ => Success())

  def beOkValue[A](expected: A): Matcher[Result[A]] =
    beOkLike((actual: A) => new BeEqualTo(expected).apply(createExpectable(actual)).toResult)

  def beOkLike[A](check: A => SpecsResult): Matcher[Result[A]] = new Matcher[Result[A]] {
    def apply[S <: Result[A]](attempt: Expectable[S]) = {
      val r = attempt.value match {
        case Ok(actual)     => check(actual)
        case Error(error)   => Failure(s"Result failed with <${Result.asString(error)}>")
      }
      result(r.isSuccess, r.message, r.message, attempt)
    }
  }
}
