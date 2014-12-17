package com.ambiata.mundane.trace

import com.ambiata.mundane.control._

import scalaz._, Scalaz._, effect._

/*
 * A category based tracing tool, for optionally journaled logging.
 *
 * This construction is specifically designed to be carried around in
 * a ReaderT sharing the same 'F', this allows for pure computations
 * to be logged via Writer and allows them to be flushed immediately
 * when in IO.
 */
case class Trace[F[_]](trace: (Trace.Key, String) => F[Unit]) {
  def apply(key: Trace.Key, message: String): F[Unit] =
    trace(key, message)

  def not(keys: List[Trace.Key])(implicit F: Applicative[F]): Trace[F] = {
    val exclude = keys.toSet
    filter(k => !exclude.contains(k))
  }

  def only(keys: List[Trace.Key])(implicit F: Applicative[F]): Trace[F] = {
    val include = keys.toSet
    filter(include.contains _)
  }

  def filter(pred: Trace.Key => Boolean)(implicit F: Applicative[F]): Trace[F] =
    Trace((k, s) => if (pred(k)) trace(k, s) else ().pure[F])
}

object Trace {
  type WriterS[A] = Writer[Vector[String], A]
  type WriterKS[A] = Writer[Vector[(Key, String)], A]

  case class Key(name: String)

  def log[F[_]](k: Key, message: String): Kleisli[F, Trace[F], Unit] =
    Kleisli[F, Trace[F], Unit](_(k, message))

  def io[A](a: => Kleisli[WriterKS, Trace[WriterKS], A]): Kleisli[RIO, Trace[RIO], A] =
    Kleisli[RIO, Trace[RIO], A](trace => {
      val w = Trace.writerK[Id]
      val (l, v) = a.run(w).run
      l.traverseU({ case (k, s) => trace(k, s) }).as(v)
    })

  def empty[F[_]: Applicative]: Trace[F] =
    Trace[F]((_, _) => ().pure[F])

  def stream(out: java.io.PrintStream) : Trace[RIO] =
    Trace[RIO]((_, s) => ResultT.fromIO { IO { out.println(s) } })

  def out: Trace[RIO] =
    stream(Console.out)

  def err: Trace[RIO] =
    stream(Console.err)

  def writer[F[_]: Applicative]: Trace[({ type l[a] = WriterT[F, Vector[String], a] })#l] =
    Trace[({ type l[a] = WriterT[F, Vector[String], a] })#l]((_, s) => WriterT.put[F, Vector[String], Unit](().pure[F])(Vector(s)))

  def writerK[F[_]: Applicative]: Trace[({ type l[a] = WriterT[F, Vector[(Key, String)], a] })#l] =
    Trace[({ type l[a] = WriterT[F, Vector[(Key, String)], a] })#l]((k, s) => WriterT.put[F, Vector[(Key, String)], Unit](().pure[F])(Vector(k -> s)))
}
