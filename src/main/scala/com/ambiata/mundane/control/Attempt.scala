package com.ambiata.mundane.control

import scala.util.control.NonFatal
import scalaz._, Scalaz._, \&/._

/**
 * Something that is attempted.
 *
 * It is either:
 *
 *  - a value of type A
 *  - an exception and or a message
 */
case class Attempt[+A](run: These[String, Throwable] \/ A) {
  def map[B](f: A => B): Attempt[B] =
    Attempt(run.map(f))

  def flatMap[B](f: A => Attempt[B]): Attempt[B] =
    Attempt(run.flatMap(a => f(a).run))

  def mapError(f: These[String, Throwable] => These[String, Throwable]): Attempt[A] =
    Attempt(run.leftMap(f))

  def isOk =
    run.isRight

  def isError =
    run.isLeft

  def toOption =
    run.toOption

  def toEither =
    run.toEither

  def toOptionError =
    run.swap.toOption

  def toOptionErrorMessage =
    run.swap.toOption.map(Attempt.asString)

  def toDisjunction =
    run

  def getOrElse[AA >: A](otherwise: => AA): AA =
    toOption.getOrElse(otherwise)

  def |||[AA >: A](otherwise: => Attempt[AA]): Attempt[AA] =
    if (isOk) this else otherwise
}

object Attempt {
  object Ok {
    def unapply[A](attempt: Attempt[A]) =
      attempt.toOption
  }

  object Error {
    def unapply[A](attempt: Attempt[A]) =
      attempt.toOptionError
  }

  object ErrorMessage {
    def unapply[A](attempt: Attempt[A]) =
      attempt.toOptionErrorMessage
  }

  def safe[A](thunk: => A): Attempt[A] =
    try ok(thunk) catch { case NonFatal(t) => exception(t) }

  def ok[A](value: A): Attempt[A] =
    Attempt(value.right)

  def exception[A](t: Throwable): Attempt[A] =
    Attempt(That(t).left)

  def fail[A](message: String): Attempt[A] =
    Attempt(This(message).left)

  def error[A](message: String, t: Throwable): Attempt[A] =
    these(Both(message, t))

  def these[A](both: These[String, Throwable]): Attempt[A] =
    Attempt(both.left)

  def asString(these: These[String, Throwable]) = these match {
    case (This(x)) => x
    case (That(x)) => x.toString()
    case (Both(x, _)) => x
  }

  def prependThis(these: These[String, Throwable], prepend: String): These[String, Throwable] =
    these.fold(m      => This(prepend + " - " + m),
      t      => Both(prepend, t),
      (m, t) => Both(prepend + " - " + m, t))

  implicit def AttemptMonad: Monad[Attempt] = new Monad[Attempt] {
    def point[A](v: => A) = ok(v)
    def bind[A, B](m: Attempt[A])(f: A => Attempt[B]) = m.flatMap(f)
  }

  implicit def AttemptEqual[A: Equal]: Equal[Attempt[A]] = {
    implicit def ThrowableEqual = Equal.equalA[Throwable]
    implicitly[Equal[These[String, Throwable] \/ A]].contramap(_.run)
  }
}
