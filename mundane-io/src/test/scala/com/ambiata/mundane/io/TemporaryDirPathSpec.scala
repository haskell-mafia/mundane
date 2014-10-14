package com.ambiata.mundane.io

import com.ambiata.mundane.control._
import com.ambiata.mundane.io.Temporary._
import com.ambiata.mundane.testing.ResultTIOMatcher._

import org.specs2.Specification

import scalaz.{Store => _, _}, Scalaz._

class TemporaryDirPathSpec extends Specification { def is = s2"""

 TemporaryDirPath should clean up its own resources
 ===================================================

   directory exists             $exists
   directory is cleaned up      $clean
   handles error                $handlesFail

"""

  def exists = {
    (for {
      e <- TemporaryDirPath.withDirPath(path => for {
        _ <- println(s"paht yo : ${path.path}").pure[ResultTIO]
        exists <- Directories.exists(path)
      } yield exists)
    } yield e) must beOkValue(true)
  }

  def clean = {
    val dir: DirPath = uniqueDirPath
    (for {
      x <- TemporaryDirPath.runWithDirPath(dir)(path => for {
        _ <- Files.write(path </> FilePath("foo"), "")
        _ <- Files.write(path </> DirPath("bar") </> FilePath("foo"), "")
        f <- Files.exists(path </> FilePath("foo"))
        b <- Files.exists(path </> DirPath("bar") </> FilePath("foo"))
      } yield f -> b)
      df <- Files.exists(dir </> FilePath("foo"))
      db <- Files.exists(dir </> DirPath("bar") </> FilePath("foo"))
    } yield (x, df -> db)) must beOkValue(((true, true), (false, false)))
  }

  def handlesFail = {
    val dir: DirPath = uniqueDirPath
    TemporaryDirPath.runWithDirPath(dir)(_ => ResultT.fail("")).toOption.unsafePerformIO() must beNone
    !dir.toFile.exists()
  }
}