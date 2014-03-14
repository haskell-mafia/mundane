package com.ambiata.mundane
package io

import java.util.UUID
import java.io._
import org.specs2._
import org.scalacheck._, Arbitrary._
import scalaz._, Scalaz._
import scalaz.effect.IO

class DirectoriesSpec extends Specification with ScalaCheck { def is = s2"""

Directories
-----------

  should list all files                     $list
  should recursively delete all files       $delete
  should determine if directory exists      $exists
  should determine if directory not exists  $notExists

"""

  implicit def ArbitraryUUID: Arbitrary[UUID] =
    Arbitrary(arbitrary[Int] map (_ => UUID.randomUUID))

  lazy val tmp = new File(System.getProperty("java.io.tmpdir", "/tmp"))

  def paths(files: List[File]): List[String] =
    files.map(_.getAbsolutePath).sorted

  def list =
    prop((tree: FileTree, uuid: UUID) => {
      val base = new File(tmp, s"${uuid}")
      val action = tree.create(base) >> Directories.list(base)
      try paths(action.unsafePerformIO) must_== paths(tree.files(base))
      finally Directories.delete(base).unsafePerformIO
    })

  def delete =
    prop((tree: FileTree, uuid: UUID) => {
      val base = new File(tmp, s"${uuid}")
      val action = tree.create(base) >> Directories.delete(base) >> Directories.exists(base)
      action.unsafePerformIO must beFalse
    })

  def exists =
    prop((uuid: UUID) => {
      val base = new File(tmp, s"${uuid}")
      val action = Directories.mkdirs(base) >> Directories.exists(base)
      try action.unsafePerformIO must beTrue
      finally Directories.delete(base).unsafePerformIO
    })

  def notExists =
    prop((uuid: UUID) => {
      val base = new File(tmp, s"${uuid}")
      Directories.exists(base).unsafePerformIO must beFalse
    })
}
