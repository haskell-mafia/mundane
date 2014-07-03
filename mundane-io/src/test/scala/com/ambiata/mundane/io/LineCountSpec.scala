package com.ambiata.mundane
package io

import com.ambiata.mundane.control._
import com.ambiata.mundane.data.Lists
import com.ambiata.mundane.testing.ResultTIOMatcher._

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
  def withTestFile[A](data: List[Int])(run: File => IO[A]): ResultTIO[A] = Temporary.using { tmp =>
    val file = (tmp </> scala.util.Random.nextInt.toString).toFile
    Streams.write(new FileOutputStream(file), Lists.prepareForFile(data.map(_.toString))) >> ResultT.fromIO(run(file))
  }

  def memory =
    prop((data: List[Int]) => withTestFile(data) { file =>
      LineCount.count(file).map(_.count)
    } must beOkValue(data.size))

  def wc =
    prop((data: List[Int]) => withTestFile(data) { file =>
      LineCount.count(file).map(_.count -> Process(s"wc -l ${file.getAbsolutePath}").!!.split("\\s+").toList.map(_.trim).filter(!_.isEmpty).head.toInt)
    } must beOkLike(counts => counts._1 === counts._2))
}
