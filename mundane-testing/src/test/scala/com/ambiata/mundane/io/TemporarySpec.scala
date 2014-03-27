package com.ambiata.mundane
package io

import com.ambiata.mundane.testing.ResultTIOMatcher._

import java.io._
import java.util.UUID

import org.specs2._, matcher._, specification._
import org.scalacheck._, Arbitrary._

import scalaz._, Scalaz._

class TemporarySpec extends Specification with ScalaCheck with AfterExample { def is = isolated ^ s2"""

Temporary
---------

  always gives you a sub directory of base      $sub
  always gives you a different filename         $different

"""

  val work = System.getProperty("java.io.tmpdir", "/tmp") </> s"TemporarySpec.${UUID.randomUUID}"

  implicit def FileArbitrary: Arbitrary[File] =
    Arbitrary(arbitrary[Int] map (n => new File(n.toString)))

  def sub =
    prop((prefix: File) =>
      Temporary.directory(work, prefix.getName).map(_.file.dirname) must beOkValue(work))

  def different =
    prop((prefix: File) => {
      Temporary.directory(work, prefix.getName) must beOkLike((t: Temporary) =>
          Temporary.directory(work, prefix.getName) must beOkLike((u: Temporary) =>
            t must_!= u))
    })

  def after =
    Directories.delete(work).run.unsafePerformIO

}
