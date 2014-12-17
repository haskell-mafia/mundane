package com.ambiata.mundane.io

import com.ambiata.mundane.control._
import com.ambiata.mundane.io.Temporary._
import com.ambiata.mundane.testing.RIOMatcher._

import org.specs2.Specification

import scalaz.{Store => _, _}, Scalaz._, effect.IO

class TemporaryLocalDirectorySpec extends Specification { def is = s2"""

 TemporaryLocalDirectory should clean up its own resources
 ===================================================

   directory exists             $exists
   directory is cleaned up      $clean
   handles error                $handlesFail

"""

  def exists = {
    (for {
      e <- TemporaryDirPath.withDirPath(path => for {
        _ <- println(s"paht yo : ${path.path}").pure[RIO]
        exists <- Directories.exists(path)
      } yield exists)
    } yield e) must beOkValue(true)
  }

  def clean = {
    val dir: LocalDirectory = uniqueLocalDirectory
    (for {
      x <- TemporaryLocalDirectory.runWithLocalDirectory(dir)(path => for {
        _ <- Files.write(path </> LocalFile.unsafe("foo"), "")
        _ <- Files.write(path </> LocalDirectory.unsafe("bar") </> LocalFile.unsafe("foo"), "")
        f <- Files.exists(path </> LocalFile.unsafe("foo"))
        b <- Files.exists(path </> LocalDirectory.unsafe("bar") </> LocalFile.unsafe("foo"))
      } yield f -> b)
      df <- Files.exists(dir </> LocalFile.unsafe("foo"))
      db <- Files.exists(dir </> LocalDirectory.unsafe("bar") </> LocalFile.unsafe("foo"))
    } yield (x, df -> db)) must beOkValue(((true, true), (false, false)))
  }

  def handlesFail = {
    val dir: LocalDirectory = uniqueLocalDirectory
    TemporaryDirPath.runWithDirPath(dir)(_ => RIO.fail[Int]("")).toOption must beNone
    !dir.toFile.exists()
  }
}
