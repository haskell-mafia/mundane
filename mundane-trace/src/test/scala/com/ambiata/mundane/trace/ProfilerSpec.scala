package com.ambiata.mundane.trace

import com.ambiata.mundane.testing.ResultTIOMatcher._
import com.ambiata.mundane.data.Lists
import com.ambiata.mundane.control._

import java.io.{ByteArrayOutputStream, PrintStream}

import org.specs2._
import org.scalacheck._, Arbitrary._

import scalaz._, Scalaz._, effect.IO

class ProfilerSpec extends Specification with ScalaCheck { def is = s2"""

Profiler Constructors
---------------------

  Profiler.empty doesn't do anything              $empty
  Profiler.tree records sections                  $sections
  Profiler.tree records counters                  $counters
  Profiler.tree records nested sections           $nested
  Profiler.tree records siblings                  $siblings

Profiler Combinators
--------------------

  'tick' upticks by 1                             $tick
  'start' only results in nothing                 $start
  'end' only results in error                     $endx
  'start' and 'end' results in tag                $both

"""
  import Profiler.{Section, Counter, Profile, ThreadId, Timestamp}

  val s = Section("section")
  val sn = Section("nested")
  val ss = Section("sibling")
  val c = Counter("counter")

  def tid =
    ThreadId(Thread.currentThread.getId)

  def empty = prop((n: Int) => {
    val p = Profiler.empty[Id]
    (for {
      _ <- p.wrap(s)(n)
      _ <- p.tick(c)
      r <- p.done()
    } yield r) must_== Profile(Nil, Map.empty) })

  def sections = prop((n: Int) =>
    run(p => p.wrap(s)(n)) must
      beOkLike(p => p.sections.exists(t => t.tid == tid && t.context == Vector(s))))

  def nested = prop((n: Int) =>
    run(p => p.bracket(s)(p.wrap(sn)(n))) must
      beOkLike(p => p.sections.map(_.context) must_== List(List(s), List(s, sn))))

  def siblings = prop((n: Int) =>
    run(p => p.wrap(s)(n) >> p.wrap(ss)(n)) must
      beOkLike(p => p.sections.map(_.context) must_== List(List(s), List(ss))))

  def counters = prop((n: Long) =>
    run(p => p.counter(c, n)) must
      beOkValue(Profile(Nil, Map(c -> n))))

  def tick = prop((n: Byte) => n.toInt > 0 ==> {
    run(p => (1 to n.toInt).toList.traverse(_ => p.tick(c))) must
      beOkValue(Profile(Nil, Map(c -> n.toLong))) })

  def start = prop((n: Int) =>
    run(p => p.start(s) >> n.pure[ResultTIO]) must
     beOkLike(p => p.sections.isEmpty))

  def endx = prop((n: Int) =>
    run(p => n.pure[ResultTIO] >> p.end(s)) must beOk.not)

  def both = prop((n: Int) =>
    run(p => p.start(s) >> n.pure[ResultTIO] >> p.end(s)) must
     beOkLike(p => p.sections.exists(t => t.tid == tid && t.context == Vector(s))))

  def run[A](f: Profiler[ResultTIO] => ResultT[IO, A]): ResultT[IO, Profile] = for {
    p <- Profiler.tree
    _ <- f(p)
    r <- p.done()
  } yield r

}
