package com.ambiata.mundane
package control

import testing.Arbitraries._
import testing.Laws._
import org.specs2._, specification._, matcher._
import scalaz._, Scalaz._, \&/._

class ResultSpec extends Specification with ScalaCheck { def is = s2"""

 Result Laws
 ===========

   equals laws                    ${equal.laws[Result[Int]]}
   monad laws                     ${monad.laws[Result]}


 Result Combinators
 ==================

   isOk                           $isOk
   isError                        $isError
   isError != isOk                $isOkExclusive
   mapError                       $mapError
   toOption Some case             $okToOption
   toOption None case             $errorToOption
   ||| ok case                    $okOr
   ||| error case                 $errorOr
   getOrElse ok case              $okGetOrElse
   getOrElse error case           $errorGetOrElse
   disjunction conversions        $disjunction

 Result Construction
 ===================
   exception safety (ok)          $safe
   exception safety (exception)   $exception
   option safety (none)           $nullage
   option safety (some)           $some

"""
  type Fail = String \&/ Throwable

  def isOk = prop((a: Int) =>
    Result.ok(a).isOk)

  def isError = prop((a: Fail) =>
    Result.these(a).isError)

  def isOkExclusive = prop((a: Result[Int]) =>
    a.isOk != a.isError)

  def mapError = prop((a: Fail, b: Fail) =>
    Result.these(a).mapError(_ => b) == Result.these(b))

  def okToOption = prop((a: Int) =>
    Result.ok(a).toOption == Some(a))

  def errorToOption = prop((a: Fail) =>
    Result.these(a).toOption == None)

  def okOr = prop((a: Int, b: Result[Int]) =>
    (Result.ok(a) ||| b) == Result.ok(a))

  def errorOr = prop((a: Fail, b: Result[Int]) =>
    (Result.these(a) ||| b) == b)

  def okGetOrElse = prop((a: Int, b: Int) =>
    Result.ok(a).getOrElse(b) == a)

  def errorGetOrElse = prop((a: Fail, b: Int) =>
    Result.these(a).getOrElse(b) == b)

  def disjunction = prop((a: Fail \/ Int) =>
    Result.fromDisjunction(a).toDisjunction == a)

  def safe = prop((a: Int) =>
    Result.safe(a) == Result.ok(a))

  def exception = prop((t: Throwable) =>
    Result.safe(throw t) == Result.exception(t))

  def some = prop((a: Int) =>
    Result.option(a) == Result.ok(Some(a)))

  def nullage = prop((_: Unit) =>
    Result.option(null) == Result.ok(None))
}
