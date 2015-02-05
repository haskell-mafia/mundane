package com.ambiata.mundane.testing

import com.ambiata.mundane.control.{Error, Ok, Result}
import org.specs2.execute.{Result => SpecsResult, Error => _, _}
import org.specs2.matcher._

import scalaz.\&/
import scalaz.\&/.This

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

  def beFail[A]: Matcher[Result[A]] =
    beFailLike(_ => Success())

  def beFailWithMessage[A](expected: String): Matcher[Result[A]] =
    beFailWith(This(expected))

  def beFailWith[A](these: String \&/ Throwable): Matcher[Result[A]] =
    beFailLike((actual: String \&/ Throwable) => new BeEqualTo(these).apply(createExpectable(actual)).toResult)

  def beFailLike[A](check: String \&/ Throwable => SpecsResult): Matcher[Result[A]] = new Matcher[Result[A]] {
    def apply[S <: Result[A]](attempt: Expectable[S]) = {
      val r: SpecsResult = attempt.value match {
        case Ok(value)    => Failure(s"Failure: Result ok with value <$value>")
        case Error(error) => check(error)
      }
      result(r.isSuccess, r.message, r.message, attempt)
    }
  }
}
