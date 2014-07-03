package com.ambiata.mundane
package io

import com.ambiata.mundane.control._
import com.ambiata.mundane.testing.ResultTIOMatcher._

import java.io._

import org.specs2._
import org.scalacheck._, Arbitrary._

import scalaz._

class TemporarySpec extends Specification with ScalaCheck { def is = s2"""

Temporary
---------

  always gives you a sub directory of base      $sub
  always gives you a different filename         $different
  using cleans up                               $usingOk
  using cleans up on error                      $usingFail

"""

  implicit def FileArbitrary: Arbitrary[File] =
    Arbitrary(arbitrary[Int] map (n => new File(n.toString)))

  def sub =
    prop((prefix: File) => Temporary.using { work =>
        Temporary.directory(work, prefix.getName).map(_.file.dirname -> work)
    } must beOkLike(dirs => dirs._1 ==== dirs._2))

  def different =
    prop((prefix: File) => Temporary.using { work =>
      Temporary.directory(work, prefix.getName) zip Temporary.directory(work, prefix.getName)
    } must beOkLike(dirs => dirs._1 must_!= dirs._2))

  def usingOk =
    Temporary.using(file => ResultT.ok(file.toFile.exists -> file))
      .map(f => f._1 -> f._2.toFile.exists) must beOkValue(true -> false)

  def usingFail = {
    var file: FilePath = null
    Temporary.using{f => file = f; ResultT.fail("")}.toOption.unsafePerformIO() must beNone
    !file.toFile.exists
  }
}
