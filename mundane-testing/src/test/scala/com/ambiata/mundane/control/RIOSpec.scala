package com.ambiata.mundane.control

import com.ambiata.mundane.control.RIOSpec._
import com.ambiata.mundane.testing.Arbitraries._
import com.ambiata.mundane.testing.Laws._
import com.ambiata.mundane.testing.RIOMatcher._
import org.specs2._, specification._, matcher._
import scalaz._, Scalaz._, \&/._, effect.IO

class RIOSpec extends Specification with ScalaCheck { def is = s2"""

 RIO Laws
 ============

   equals laws                    ${equal.laws[RIO[Int]]}
   monad laws                     ${monad.laws[({ type l[a] = RIO[a] })#l]}


 RIO Combinators
 ===================

   ||| ok case                       $okOr
   ||| ok case with side effects     $okOrIO
   ||| error case                    $errorOr
   disjunction conversions           $disjunction
   disjunction string conversions    $disjunctionString
   disjunction throwable conversions $disjunctionThrowable
   fromOption                        $fromOption
   fromOption error case             $fromOptionF
   when                              $when
   when                              $whenF
   unless                            $unless
   unless                            $unlessF

 RIO Construction
 ====================

   exception safety (ok)          $safe
   exception safety (exception)   $exception
   option safety (none)           $nullage
   option safety (some)           $some

"""
  type Fail = String \&/ Throwable

  def monads =
    monad.laws[({ type l[a] = RIO[a] })#l]

  def okOr = prop((a: Int, b: RIO[Int]) =>
    (RIO.ok[Int](a) ||| b) ===== RIO.ok[Int](a))

  def okOrIO = {
    import scalaz.effect._, com.ambiata.mundane.testing.RIOMatcher._
    var i = 0
    val result = RIO.fromIO(IO { i = i + 1; i })
    (result ||| result) must beOkValue(1)
  }

  def errorOr = prop((a: Fail, b: RIO[Int]) =>
    (RIO.these[Int](a) ||| b) ===== b)

  def disjunction = prop((a: Fail \/ Int) =>
    RIO.fromDisjunction[Int](a).runT.toDisjunction.unsafePerformIO ==== a)

  def disjunctionString = prop((a: String \/ Int) =>
    RIO.fromDisjunctionString[Int](a).runT.toDisjunction.unsafePerformIO ==== a.leftMap[These[String, Throwable]](This.apply))

  def disjunctionThrowable = prop((a: Throwable \/ Int) =>
    RIO.fromDisjunctionThrowable[Int](a).runT.toDisjunction.unsafePerformIO ==== a.leftMap[These[String, Throwable]](That.apply))

  def fromOption =
    RIO.fromOption[Int](Some(1), "foo") ===== RIO.ok(1)

  def fromOptionF =
    RIO.fromOption[Int](None, "foo") ===== RIO.fail("foo")

  def when =
    RIO.when(false, RIO.fail("foo")) ===== RIO.unit

  def whenF =
    RIO.when(true, RIO.fail("foo")) ===== RIO.fail("foo")

  def unless =
    RIO.unless(true, RIO.fail("foo")) ===== RIO.unit

  def unlessF =
    RIO.unless(false, RIO.fail("foo")) ===== RIO.fail("foo")

  def safe = prop((a: Int) =>
    RIO.safe[Int](a) ===== RIO.ok[Int](a))

  def exception = prop((t: Throwable) =>
    RIO.safe[Int](throw t) ===== RIO.exception[Int](t))

  def some = prop((a: Int) =>
    RIO.option[Int](a) ===== RIO.ok[Option[Int]](Some(a)))

  def nullage = prop((_: Unit) =>
    RIO.option[String](bad) ===== RIO.ok[Option[String]](None))

  def bad: String = null

  implicit class Foo[A](a: RIO[A]) {
    def =====(b: RIO[A]): org.specs2.execute.Result =
      a.unsafePerformIO ==== b.unsafePerformIO
  }
}

object RIOSpec {
  implicit def RIOEqual[A]: Equal[RIO[Int]] =
    Equal.equal((a, b) => a.unsafePerformIO === b.unsafePerformIO)

}
