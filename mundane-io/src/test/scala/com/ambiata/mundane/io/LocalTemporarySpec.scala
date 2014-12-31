package com.ambiata.mundane.io

import com.ambiata.disorder._
import com.ambiata.mundane.control._
import com.ambiata.mundane.testing.RIOMatcher._
import com.ambiata.mundane.io.Arbitraries._

import org.specs2._

import scalaz.{Store => _, _}, Scalaz._, effect.IO

class LocalTemporarySpec extends Specification with ScalaCheck { def is = s2"""

 Temporary should clean up its own resources
 ===========================================

   clean up a file                                   $file
   clean up a directory                              $directory
   get a temporary file that exists                  $getFile
   get a temporary directory that exists             $getDirectory
   no conflicts                                      $conflicts

"""
  /** Testing Temporary clean up finalizers */
  def file = prop((data: String, tmp: LocalTemporary) => for {
    f <- tmp.fileWithContent(data)
    e <- Files.exists(f)
    _ <- RIO.unsafeFlushFinalizers
    z <- Files.exists(f)
  } yield e -> z ==== true -> false)

  /** Testing Temporary clean up finalizers */
  def directory = prop((data: String, id: Ident, local: LocalTemporary) => for {
    d <- local.directory
    f = d </> FilePath.unsafe(id.value)
    _ <- Files.write(f , data)
    e <- Files.exists(f)
    _ <- RIO.unsafeFlushFinalizers
    z <- Directories.exists(d)
  } yield e -> z ==== true -> false)

  def getFile = prop((data: String, local: LocalTemporary) => for {
    f <- local.fileWithContent(data)
    e <- Files.exists(f)
    d <- Files.read(f)
  } yield e -> d ==== true -> data)

  def getDirectory = prop((local: LocalTemporary) => for {
    d <- local.directoryThatExists
    e <- Directories.exists(d)
  } yield e ==== true)

  def conflicts = prop((local: LocalTemporary, i: NaturalInt) => i.value > 0 ==> (for {
    l <- (1 to i.value % 100).toList.traverse(i => if (i % 2 == 0) local.directory.map(_.path) else local.file.map(_.path))
  } yield l.distinct ==== l))
}
