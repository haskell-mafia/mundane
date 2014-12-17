package com.ambiata.mundane.io

import com.ambiata.mundane.control._
import com.ambiata.mundane.io.Temporary._
import com.ambiata.mundane.testing.RIOMatcher._

import org.specs2.Specification

import scalaz.{Store => _, _}, Scalaz._, effect.IO

class TemporaryLocalFileSpec extends Specification { def is = s2"""

 TemporaryLocalFile should clean up its own resources
 ===================================================

   single file               $singleFile
   handles error             $handlesFail

"""

  def singleFile = {
    val p = uniqueLocalFile
    (for {
      x <- TemporaryLocalFile.runWithLocalFile(p)(path => for {
        _ <- Files.write(path, "")
        e <- Files.exists(path)
      } yield e)
      y <- Files.exists(p)
    } yield x -> y) must beOkValue(true -> false)
  }

  def handlesFail = {
    val file: LocalFile = uniqueLocalFile
    TemporaryFilePath.runWithFilePath(file)( _ => RIO.fail[Int]("")).toOption must beNone
    !file.toFile.exists()
  }
}
