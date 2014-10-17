package com.ambiata.mundane.io

import com.ambiata.mundane.control._
import com.ambiata.mundane.io.Temporary._
import com.ambiata.mundane.testing.ResultTIOMatcher._

import org.specs2.Specification

import scalaz.{Store => _, _}, Scalaz._, effect.IO

class TemporaryDirPathSpec extends Specification { def is = s2"""

 TemporaryDirPath should clean up its own resources
 ===================================================

   directory exists             $exists
   directory is cleaned up      $clean
   handles error                $handlesFail

"""

  def exists =
    TemporaryDirPath.withDirPath(Directories.exists) must beOkValue(true)

  def clean = {
    val dir: DirPath = uniqueDirPath
    (for {
      x <- TemporaryDirPath.runWithDirPath(dir)(path => for {
        _ <- Files.write(path </> FilePath.unsafe("foo"), "")
        _ <- Files.write(path </> DirPath.unsafe("bar") </> FilePath.unsafe("foo"), "")
        f <- Files.exists(path </> FilePath.unsafe("foo"))
        b <- Files.exists(path </> DirPath.unsafe("bar") </> FilePath.unsafe("foo"))
      } yield f -> b)
      df <- Files.exists(dir </> FilePath.unsafe("foo"))
      db <- Files.exists(dir </> DirPath.unsafe("bar") </> FilePath.unsafe("foo"))
    } yield (x, df -> db)) must beOkValue(((true, true), (false, false)))
  }

  def handlesFail = {
    val dir: DirPath = uniqueDirPath
    TemporaryDirPath.runWithDirPath(dir)(_ => ResultT.fail[IO, Int]("")).toOption.unsafePerformIO() must beNone
    !dir.toFile.exists()
  }
}
