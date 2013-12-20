package com.ambiata
package mundane
package testing

import control._
import org.specs2._
import matcher._
import execute._

object AttemptMatcher extends ThrownExpectations {
  def beOk[A]: Matcher[Attempt[A]] =
    beOkLike(_ => Success())

  def beOkValue[A](expected: A): Matcher[Attempt[A]] =
    beOkLike((actual: A) => new BeEqualTo(expected).apply(createExpectable(actual)).toResult)

  def beOkLike[A](check: A => Result): Matcher[Attempt[A]] = new Matcher[Attempt[A]] {
    def apply[S <: Attempt[A]](attempt: Expectable[S]) = {
      val r = attempt.value match {
        case Attempt.Ok(actual)          => check(actual)
        case Attempt.ErrorMessage(error) => Failure(s"Attempt failed with <$error>")
      }
      result(r.isSuccess, r.message, r.message, attempt)
    }
  }
}
