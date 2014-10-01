package com.ambiata.mundane
package testing

import com.ambiata.mundane.control._
import org.specs2._, matcher._, execute.{Result => SpecsResult, Error => SpecsError, _}
import scalaz.{Success => _, Failure => _, _}, effect.IO, \&/._

object ResultTIOMatcher extends ThrownExpectations {
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

  def beFail[A]: Matcher[ResultT[IO, A]] =
    beFailLike(_ => Success())

  def beFailWithMessage[A](expected: String): Matcher[ResultT[IO, A]] =
    beFailWith(This(expected))

  def beFailWith[A](these: String \&/ Throwable): Matcher[ResultT[IO, A]] =
    beFailLike((actual: String \&/ Throwable) => new BeEqualTo(these).apply(createExpectable(actual)).toResult)

  def beFailLike[A](check: String \&/ Throwable => SpecsResult): Matcher[ResultT[IO, A]] = new Matcher[ResultT[IO, A]] {
    def apply[S <: ResultT[IO, A]](attempt: Expectable[S]) = {
      val r: SpecsResult = attempt.value.run.unsafePerformIO match {
        case Ok(value)    => Failure(s"Failure: Result ok with value <$value>")
        case Error(error) => check(error)
      }
      result(r.isSuccess, r.message, r.message, attempt)
    }
  }
}
