package com.ambiata.mundane
package io

import java.io._
import org.specs2._
import org.scalacheck._, Arbitrary._

import scalaz._, Scalaz._

class TemporarySpec extends Specification with ScalaCheck { def is = s2"""

Temporary
---------

  always gives you a sub directory of base      $sub
  always gives you a different filename         $different

"""
  lazy val tmp = new File(System.getProperty("java.io.tmpdir", "/tmp"))

  implicit def FileArbitrary: Arbitrary[File] =
    Arbitrary(arbitrary[Int] map (n => new File(n.toString)))

  def sub =
    prop((prefix: File) => {
      val dir = Temporary.directory(tmp, prefix.getName).unsafePerformIO
      try dir.file.getParentFile must_== tmp
      finally dir.clean.unsafePerformIO
    })

  def different =
    prop((prefix: File) => {
      val dir1 = Temporary.directory(tmp, prefix.getName).unsafePerformIO
      val dir2 = Temporary.directory(tmp, prefix.getName).unsafePerformIO
      try dir1 must_!= dir2
      finally { (dir1.clean >> dir2.clean).unsafePerformIO }
    })
}
