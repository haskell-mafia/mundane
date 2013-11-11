package com.ambiata.mundane.data

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scalaz.Monad

object Futures {
  /** implementation of Monad for a Future */
  implicit def futureHasMonad: Monad[Future] = new Monad[Future] {
    def point[A](a: => A) = Future(a)

    def bind[A, B](fa: Future[A])(f: (A) => Future[B]) = for {
      a <- fa
      b <- f(a)
    } yield b
  }
}
