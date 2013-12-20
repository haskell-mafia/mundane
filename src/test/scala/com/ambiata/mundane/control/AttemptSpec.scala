package com.ambiata.mundane
package control

import testing.Arbitraries._
import testing.Laws._
import org.specs2._, specification._, matcher._
import scalaz._, Scalaz._, \&/._

class AttemptSpec extends Specification with ScalaCheck { def is = s2"""

 Attempt Laws
 ===============

   equals laws                    ${equal.laws[Attempt[Int]]}
   monad laws                     ${monad.laws[Attempt]}


 Attempt Combinators
 ======================

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
   run                            $run
   toDisjunction is alias of run  $toDisjunction

"""
  type Error = String \&/ Throwable

  def isOk = prop((a: Int) =>
    Attempt.ok(a).isOk)

  def isError = prop((a: Error) =>
    Attempt.these(a).isError)

  def isOkExclusive = prop((a: Attempt[Int]) =>
    a.isOk != a.isError)

  def mapError = prop((a: Error, b: Error) =>
    Attempt.these(a).mapError(_ => b) == Attempt.these(b))

  def okToOption = prop((a: Int) =>
    Attempt.ok(a).toOption == Some(a))

  def errorToOption = prop((a: Error) =>
    Attempt.these(a).toOption == None)

  def okOr = prop((a: Int, b: Attempt[Int]) =>
    (Attempt.ok(a) ||| b) == Attempt.ok(a))

  def errorOr = prop((a: Error, b: Attempt[Int]) =>
    (Attempt.these(a) ||| b) == b)

  def okGetOrElse = prop((a: Int, b: Int) =>
    Attempt.ok(a).getOrElse(b) == a)

  def errorGetOrElse = prop((a: Error, b: Int) =>
    Attempt.these(a).getOrElse(b) == b)

  def run = prop((a: Error \/ Int) =>
    Attempt(a).run == a)

  def toDisjunction = prop((a: Attempt[Int]) =>
    a.run == a.toDisjunction)
}
