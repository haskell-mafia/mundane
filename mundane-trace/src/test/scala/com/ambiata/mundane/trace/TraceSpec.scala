package com.ambiata.mundane.trace

import com.ambiata.mundane.testing.RIOMatcher._
import com.ambiata.mundane.data.Lists
import com.ambiata.mundane.control._

import java.io.{ByteArrayOutputStream, PrintStream}

import org.specs2._
import org.scalacheck._, Arbitrary._

import scalaz._, Scalaz._

class TraceSpec extends Specification with ScalaCheck { def is = s2"""

Trace Constructors
------------------

  Trace.writer accumulates messages               $writer
  Trace.empty doesn't do anything                 $empty
  Trace.stream accumulates in stream              $stream

Trace Combinators
-----------------

  'not' acts as blacklist                         $blacklist
  'only' acts as whitelist                        $whitelist
  'filter(const true)' accumulates all messages   $nofilter
  'filter(const false)' accumulates no messages   $filter

Trace Demonstration
-------------------

  In a real application you can use pure Writer style
  logging interspersed with flushed to disk IO based
  logging $example

"""
  import Trace.{Key, WriterS}

  val one = Key("one")
  val two = Key("two")

  def writer = prop((x: Vector[String], y: Vector[String]) =>
    run(Trace.writer[Id], x, y) must_== (x ++ y))

  def empty = prop((x: Vector[String], y: Vector[String]) =>
    run(Trace.empty[WriterS], x, y) must_== Vector())

  def stream = prop((x: Vector[String]) =>
    capture(out => { val trace = Trace.stream(out); x.traverseU(trace(one, _)) })(_ must_==
     Lists.prepareForFile(x.toList)))

  def blacklist = prop((x: Vector[String], y: Vector[String]) =>
    run(Trace.writer[Id].not(List(two)), x, y) must_== x)

  def whitelist = prop((x: Vector[String], y: Vector[String]) =>
    run(Trace.writer[Id].only(List(two)), x, y) must_== y)

  def nofilter = prop((x: Vector[String], y: Vector[String]) =>
    run(Trace.writer[Id].filter(_ => true), x, y) must_== (x ++ y))

  def filter = prop((x: Vector[String], y: Vector[String]) =>
    run(Trace.writer[Id].filter(_ => false), x, y) must_== Vector())

  def example = prop((m: Int, n: Int, o: Int) =>  {
    capture(out => demo.work(m, n, o).run(Trace.stream(out)))(_ must_==
      s"""working on ${m}, ${n}, ${o}
         |adding ${m}, ${n}
         |got ${m + n}
         |adding ${m + n}, ${o}
         |got ${m + n + o}
         |done with ${m + n + o}
         |""".stripMargin) })

  object demo {
    import Trace.{Key, log, io, WriterKS}

    type App[A] = ReaderT[RIO, Trace[RIO], A]
    type Purity[A] = ReaderT[WriterKS, Trace[WriterKS], A]

    val adding = Key("adding")
    val doing = Key("doing")

    /* Simulating some effectful computation in IO. */
    def work(m: Int, n: Int, o: Int): App[Unit] = for {
      _ <- log[RIO](doing, s"working on ${m}, ${n}, ${o}")
      p <- io { add(m, n).flatMap(add(_, o)) }
      _ <- log[RIO](doing, s"done with $p")
    } yield ()

    /* Simulating some pure computation. */
    def add(m: Int, n: Int): Purity[Int] = for {
      _ <- log[WriterKS](adding, s"adding ${m}, ${n}")
      o =  m + n
      _ <- log[WriterKS](adding, s"got $o")
    } yield o
  }

  def run(trace: Trace[WriterS], x: Vector[String], y: Vector[String]): Vector[String] =
    (for {
      _ <- x.traverseU(trace(one, _))
      _ <- y.traverseU(trace(two, _))
    } yield ()).run._1

  def capture[A](f: PrintStream => RIO[A])(check: String => org.specs2.execute.Result) = {
    val baos = new ByteArrayOutputStream
    val out = new PrintStream(baos, true, "UTF-8")
    (f(out) must beOk) and check(new String(baos.toString))
  }
}
