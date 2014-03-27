package com.ambiata.mundane
package io

import com.ambiata.mundane.testing.ResultTIOMatcher._

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

  lazy val tmp = new File(System.getProperty("java.io.tmpdir", "/tmp")).getPath

  def paths(files: List[FilePath]): List[FilePath] =
    files.map(_.absolute).sortBy(_.path)

  def list =
    prop((tree: FileTree, uuid: UUID) => {
      val base = tmp </> s"${uuid}"
      val action = tree.create(base) >> Directories.list(base)
      try action.map(paths) must beOkValue(paths(tree.files(base)))
      finally clean(base)
    })

  def delete =
    prop((tree: FileTree, uuid: UUID) => {
      val base = tmp </> s"${uuid}"
      val action = tree.create(base) >> Directories.delete(base) >> Directories.exists(base)
      action must beOkValue(false)
    })

  def exists =
    prop((uuid: UUID) => {
      val base = tmp </> s"${uuid}"
      val action = Directories.mkdirs(base) >> Directories.exists(base)
      try action must beOkValue(true)
      finally clean(base)
    })

  def notExists =
    prop((uuid: UUID) => {
      val base = tmp </> s"${uuid}"
      Directories.exists(base) must beOkValue(false)
    })

  def clean(path: FilePath): Unit = {
    Directories.delete(path).run.unsafePerformIO
    ()
  }
}
