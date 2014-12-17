package com.ambiata.mundane.io

import java.io.File

import com.ambiata.mundane.control._
import com.ambiata.mundane.io.Temporary._
import com.ambiata.mundane.testing.RIOMatcher._

import org.specs2._

import scalaz.{Store => _, _}, Scalaz._, effect.IO

class TemporaryLocalPathSpec extends Specification with ScalaCheck { def is = s2"""

 TemporaryLocalPath should clean up its own resources
 ====================================================

   path is cleaned up (file)    $clean
   path is cleaned up (dir)     $cleanDir
   handles error                $handlesFailure

"""

  def clean = prop((data: String) => {
    val p = LocalPath(uniqueLocalPath)
    (for {
      z <- TemporaryLocalPath.runWithLocalPath(p)(path => for {
        _ <- path.write("asd")
        e <- path.exists
      } yield e)
      y <- p.exists
    } yield z -> y) must beOkValue(true -> false)
  })

  def cleanDir = pending

  def handlesFailure = prop((data: String) => {
    val file = uniqueLocalPath
    TemporaryLocalPath.withLocalPath(file =>
      file.write(data) >>
        ResultT.fail[IO, Unit]("")).toOption.unsafePerformIO() must beNone
    new File(file.path).exists() ==== false
  })

}
