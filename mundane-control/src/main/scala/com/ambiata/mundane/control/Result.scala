package com.ambiata.mundane.control

import com.ambiata.mundane.error.Throwables
import scala.util.control.NonFatal
import scalaz._, Scalaz._, \&/._


/**
 * A data type for holding results. This is effectively just an
 * Either with a specialized left. This particular specializtion
 * handles string/exception based failures and should be used
 * to wrap up unsafe apis (i.e. java code).
 *
 * This specialization exists for a number of reasons:
 *  - scala.
 *  - having a single type param helps inference in a non-trivial way
 *    (this is essential to it later being used in a monad transformer).
 *  - useful methods for manipulating error messages.
 *  - better pattern matching support.
 *  - and again, scala.
 */
sealed trait Result[A] {
  @inline final def fold[X](
    ok: A => X,
    error: These[String, Throwable] => X
  ): X = this match {
    case Ok(a)    => ok(a)
    case Error(e) => error(e)
  }

  @inline final def foldAll[X](
    ok: A => X,
    fail: String => X,
    exception: Throwable => X,
    both: (String, Throwable) => X
  ): X = fold(ok, _ match {
    case This(m) => fail(m)
    case That(e) => exception(e)
    case Both(m, e) => both(m, e)
  })

  def map[B](f: A => B): Result[B] =
    flatMap(f andThen Result.ok[B])

  def flatMap[B](f: A => Result[B]): Result[B] =
    fold(f, Result.these[B])

  def mapError(f: These[String, Throwable] => These[String, Throwable]): Result[A] =
    fold(Result.ok, f andThen Result.these)

  def mapErrorMessage(f: Option[String] => String): Result[A] =
    foldAll(
      Result.ok,
      m => Result.fail(f(Some(m))),
      t => Result.error(f(None), t),
      (m, t) => Result.error(f(Some(m)), t)
    )

  def prependErrorMessage(annotation: String): Result[A] =
    mapErrorMessage({
      case None          => annotation
      case Some(current) => s"${annotation} - ${current}"
    })

  def isOk: Boolean =
    fold(_ => true, _ => false)

  def isError: Boolean =
    !isOk

  def toDisjunction: These[String, Throwable] \/ A =
    fold(_.right, _.left)

  def toOption: Option[A] =
    fold(_.some, _ => none[A])

  def toEither: Either[These[String, Throwable], A] =
    toDisjunction.toEither

  def toOptionError: Option[These[String, Throwable]] =
    fold(_ => none, _.some)

  def toOptionErrorMessage: Option[String] =
    fold(_ => none, e => Result.asString(e).some)

  def getOrElse[AA >: A](otherwise: => AA): AA =
    toOption.getOrElse(otherwise)

  def |||(otherwise: => Result[A]): Result[A] =
    if (isOk) this else otherwise
}

case class Ok[A](value: A) extends Result[A]
case class Error[A](error: These[String, Throwable]) extends Result[A]

object Result {
  def safe[A](thunk: => A): Result[A] =
    try ok(thunk) catch { case NonFatal(t) => exception(t) }

  def option[A](thunk: => A): Result[Option[A]] =
    try ok(Option(thunk)) catch { case NonFatal(t) => exception(t) }

  def ok[A](a: A): Result[A] =
    Ok(a)

  def exception[A](t: Throwable): Result[A] =
    these(That(t))

  def fail[A](message: String): Result[A] =
    these(This(message))

  def error[A](message: String, t: Throwable): Result[A] =
    these(Both(message, t))

  def these[A](error: These[String, Throwable]): Result[A] =
    Error(error)

  def fromDisjunction[A](v: These[String, Throwable] \/ A): Result[A] =
    v.fold(these, ok)

  def fromEitherString[A](v: Either[String, A]): Result[A] =
    v.fold(fail, ok)

  def fromEitherThrowable[A](v: Either[Throwable, A]): Result[A] =
    v.fold(exception, ok)

  def fromDisjunctionString[A](v: String \/ A): Result[A] =
    v.fold(fail, ok)

  def fromDisjunctionThrowable[A](v: Throwable \/ A): Result[A] =
    v.fold(exception, ok)

  def asString(these: These[String, Throwable]) = these match {
    case (This(m)) => m
    case (That(t)) => Throwables.renderWithStack(t)
    case (Both(m, t)) => s"${m}, caused by:\n${Throwables.renderWithStack(t)}}"
  }

  def prependThis(these: These[String, Throwable], prepend: String): These[String, Throwable] =
    these.fold(m      => This(prepend + " - " + m),
      t      => Both(prepend, t),
      (m, t) => Both(prepend + " - " + m, t))

  implicit def ResultMonad: Monad[Result] = new Monad[Result] {
    def point[A](v: => A) = ok(v)
    def bind[A, B](m: Result[A])(f: A => Result[B]) = m.flatMap(f)
  }

  implicit def ResultEqual[A: Equal]: Equal[Result[A]] = {
    implicit def ThrowableEqual = Equal.equalA[Throwable]
    implicitly[Equal[These[String, Throwable] \/ A]].contramap(_.toDisjunction)
  }
}
