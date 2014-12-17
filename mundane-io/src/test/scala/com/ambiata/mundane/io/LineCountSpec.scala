package com.ambiata.mundane.io

import com.ambiata.disorder._
import com.ambiata.mundane.control._
import com.ambiata.mundane.data.Lists
import com.ambiata.mundane.io.Arbitraries._
import com.ambiata.mundane.testing.RIOMatcher._
import com.ambiata.mundane.path._

import java.io._

import org.specs2._

import scalaz._, Scalaz._, effect._
import scala.sys.process.Process

class LineCountSpec extends Specification with ScalaCheck { def is = s2"""

LineCount
---------

  matches in memory calculation      $memory
  matches wc -l                      $wc

"""
  def withTestFile[A](data: List[Int], local: LocalTemporary)(run: File => IO[A]): RIO[A] = for {
    p <- local.fileThatExists
    f = p.toFile
    _ <- Streams.write(new FileOutputStream(f), Lists.prepareForFile(data.map(_.toString)))
    r <- RIO.fromIO(run(f))
  } yield r

  def memory =
    prop((data: List[Int], local: LocalTemporary) => withTestFile(data, local) { file =>
      LineCount.count(file).map(_.count)
    } must beOkValue(data.size))

  def wc =
    prop((data: List[Int], local: LocalTemporary) => withTestFile(data, local) { file =>
      LineCount.count(file).map(_.count -> Process(s"wc -l ${file.getAbsolutePath}").!!.split("\\s+").toList.map(_.trim).filter(!_.isEmpty).head.toInt)
    } must beOkLike(counts => counts._1 === counts._2))
}
