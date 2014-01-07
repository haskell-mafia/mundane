package com.ambiata.mundane
package control

import testing.Arbitraries._
import testing.Laws._
import org.specs2._, specification._, matcher._
import scalaz._, Scalaz._, \&/._

class ResultTSpec extends Specification with ScalaCheck { def is = s2"""

 ResultT Laws
 ============

   equals laws                    ${equal.laws[ResultT[Option, Int]]}
   monad laws                     ${monad.laws[({ type l[a] = ResultT[Option, a] })#l]}


 ResultT Combinators
 ===================

   ||| ok case                    $okOr
   ||| error case                 $errorOr
   getOrElse ok case              $okGetOrElse
   getOrElse error case           $errorGetOrElse
   disjunction conversions        $disjunction

 ResultT Construction
 ====================

   exception safety (ok)          $safe
   exception safety (exception)   $exception
   option safety (none)           $nullage
   option safety (some)           $some

"""
  type Fail = String \&/ Throwable

  def monads =
    monad.laws[({ type l[a] = ResultT[Option, a] })#l]

  def okOr = prop((a: Int, b: ResultT[Option, Int]) =>
    (ResultT.ok[Option, Int](a) ||| b) == ResultT.ok[Option, Int](a))

  def errorOr = prop((a: Fail, b: ResultT[Option, Int]) =>
    (ResultT.these[Option, Int](a) ||| b) == b)

  def okGetOrElse = prop((a: Int, b: Int) =>
    ResultT.ok[Option, Int](a).getOrElse(b) == Some(a))

  def errorGetOrElse = prop((a: Fail, b: Int) =>
    ResultT.these[Option, Int](a).getOrElse(b) == Some(b))

  def disjunction = prop((a: Option[Fail \/ Int]) =>
    ResultT.fromDisjunction[Option, Int](a).toDisjunction == a)

  def safe = prop((a: Int) =>
    ResultT.safe[Option, Int](a) == ResultT.ok[Option, Int](a))

  def exception = prop((t: Throwable) =>
    ResultT.safe[Option, Int](throw t) == ResultT.exception[Option, Int](t))

  def some = prop((a: Int) =>
    ResultT.option[Id, Int](a) == ResultT.ok[Id, Option[Int]](Some(a)))

  def nullage = prop((_: Unit) =>
    ResultT.option[Id, String](bad) == ResultT.ok[Id, Option[String]](None))

  def bad: String = null
}
