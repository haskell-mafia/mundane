package com.ambiata.mundane.control

import scala.util.control.NonFatal
import scalaz._, Scalaz._, \&/._

/**
 * Transformer version of Result.
 */
case class ResultT[F[+_], +A](run: F[Result[A]]) {
  def map[B](f: A => B)(implicit F: Functor[F]): ResultT[F, B] =
    ResultT(run.map(_.map(f)))

  def flatMap[B](f: A => ResultT[F, B])(implicit F: Monad[F]): ResultT[F, B] =
    ResultT(run.flatMap({
      case Ok(a)    => f(a).run
      case Error(e) => Result.these(e).pure[F]
    }))

  def isOk(implicit F: Functor[F]): F[Boolean] =
    toOption.map(_.isDefined)

  def isError(implicit F: Functor[F]): F[Boolean] =
    isOk.map(!_)

  def toOption(implicit F: Functor[F]): F[Option[A]] =
    toDisjunction.map(_.toOption)

  def toDisjunction(implicit F: Functor[F]): F[These[String, Throwable] \/ A] =
    run.map(_.toDisjunction)

  def getOrElse[AA >: A](otherwise: => AA)(implicit F: Functor[F]): F[AA] =
    toOption.map(_.getOrElse(otherwise))

  def |||[AA >: A](otherwise: => ResultT[F, AA])(implicit F: Monad[F]): ResultT[F, AA] =
    ResultT[F, AA](isOk.flatMap(ok => if (ok) this.run else otherwise.run))
}

object ResultT {
  def safe[F[+_]: Monad, A](thunk: => A): ResultT[F, A] =
    ResultT[F, A](Result.safe(thunk).point[F])

  def option[F[+_]: Monad, A](thunk: => A): ResultT[F, Option[A]] =
    ResultT[F, Option[A]](Result.option(thunk).point[F])

  def ok[F[+_]: Monad, A](value: A): ResultT[F, A] =
    ResultT[F, A](Result.ok(value).point[F])

  def exception[F[+_]: Monad, A](t: Throwable): ResultT[F, A] =
    these[F, A](That(t))

  def fail[F[+_]: Monad, A](message: String): ResultT[F, A] =
    these[F, A](This(message))

  def error[F[+_]: Monad, A](message: String, t: Throwable): ResultT[F, A] =
    these[F, A](Both(message, t))

  def these[F[+_]: Monad, A](both: These[String, Throwable]): ResultT[F, A] =
    ResultT[F, A](Result.these(both).point[F])

  def fromDisjunctionF[F[+_]: Functor, A](v: F[These[String, Throwable] \/ A]): ResultT[F, A] =
    ResultT[F, A](v.map(Result.fromDisjunction))

  def fromDisjunction[F[+_]: Monad, A](v: These[String, Throwable] \/ A): ResultT[F, A] =
    fromDisjunctionF(v.point[F])

  implicit def ResultTMonad[F[+_]: Monad]: Monad[({ type l[a] = ResultT[F, a] })#l] =
    new Monad[({ type l[a] = ResultT[F, a] })#l] {
      def point[A](v: => A) = ok[F, A](v)
      def bind[A, B](m: ResultT[F, A])(f: A => ResultT[F, B]) = m.flatMap(f)
    }

  implicit def ResultTEqual[F[+_], A](implicit E: Equal[F[Result[A]]]): Equal[ResultT[F, A]] =
    implicitly[Equal[F[Result[A]]]].contramap(_.run)
}
