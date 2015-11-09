package com.ambiata.mundane.testing

import com.ambiata.mundane.control._
import org.scalacheck.Prop
import org.specs2._
import org.specs2.execute.{Error => SpecsError, Result => SpecsResult, _}
import org.specs2.matcher._

import scalaz.\&/._
import scalaz.{Failure => _, Success => _, _}

object RIOMatcher extends ThrownExpectations with ScalaCheckMatchers {
  implicit def RIOAsResult[A: AsResult]: AsResult[RIO[A]] = new AsResult[RIO[A]] {
    def asResult(t: => RIO[A]): SpecsResult = {
      t.unsafePerformIO match {
        case Ok(actual)     => AsResult(actual)
        case Error(error)   => Failure(s"Result failed with <${Result.asString(error)}>")
      }
    }
  }

  implicit def RIOProp[A : AsResult](r: => RIO[A]): Prop =
    resultProp(AsResult(r))

  def switchResult[A](result: RIO[A]): RIO[These[String, Throwable]] =
    result.unsafePerformIO match {
      case Ok(actual) =>
        RIO.failIO(s"Result was Ok $actual")
      case Error(error) =>
        RIO.these(error)
    }

  def switchResultString[A](result: RIO[A]): RIO[String] =
    result.unsafePerformIO match {
      case Ok(actual) =>
        RIO.failIO(s"Result was Ok $actual")
      case Error(error) => error match {
        case This(s) =>
          RIO.ok[String](s)
        case _ =>
          RIO.failIO(s"Result was real failure <${Result.asString(error)}>")
      }
    }

  def beOk[A]: Matcher[RIO[A]] =
    beOkLike(_ => Success())

  def beOkValue[A](expected: A): Matcher[RIO[A]] =
    beOkLike((actual: A) => new BeEqualTo(expected).apply(createExpectable(actual)).toResult)

  def beOkLike[A](check: A => SpecsResult): Matcher[RIO[A]] = new Matcher[RIO[A]] {
    def apply[S <: RIO[A]](attempt: Expectable[S]) = {
      val r = attempt.value.unsafePerformIO match {
        case Ok(actual)     => check(actual)
        case Error(error)   => Failure(s"Result failed with <${Result.asString(error)}>")
      }
      result(r.isSuccess, r.message, r.message, attempt)
    }
  }
  def beFail[A]: Matcher[RIO[A]] =
    beFailLike(_ => Success())

  def beFailWithMessage[A](expected: String): Matcher[RIO[A]] =
    beFailWith(This(expected))

  def beFailWith[A](these: String \&/ Throwable): Matcher[RIO[A]] =
    beFailLike((actual: String \&/ Throwable) => new BeEqualTo(these).apply(createExpectable(actual)).toResult)

  def beFailLike[A](check: String \&/ Throwable => SpecsResult): Matcher[RIO[A]] = new Matcher[RIO[A]] {
    def apply[S <: RIO[A]](attempt: Expectable[S]) = {
      val r: SpecsResult = attempt.value.unsafePerformIO match {
        case Ok(value)    => Failure(s"Failure: Result ok with value <$value>")
        case Error(error) => check(error)
      }
      result(r.isSuccess, r.message, r.message, attempt)
    }
  }
}
