package com.ambiata.mundane
package io

import com.ambiata.mundane.data.Lists

import java.io._

import org.specs2._
import org.scalacheck._, Arbitrary._

import scala.sys.process.Process

class LineCountSpec extends Specification with ScalaCheck { def is = s2"""

LineCount
---------

  matches in memory calculation      $memory
  matches wc -l                      $wc

"""
  def withTestFile[A](data: List[Int])(run: File => A): A = {
    val tmp = new File(System.getProperty("java.io.tmpdir", "/tmp"))
    val file = new File(tmp, scala.util.Random.nextInt.toString)
    try {
      Streams.write(new FileOutputStream(file), Lists.prepareForFile(data.map(_.toString)))
      run(file)
    } finally if (file.exists) file.delete
  }

  def memory =
    prop((data: List[Int]) => withTestFile(data) { file =>
      LineCount.count(file).unsafePerformIO.count must_== data.size
    })

  def wc =
    prop((data: List[Int]) => withTestFile(data) { file =>
      LineCount.count(file).unsafePerformIO.count must_== Process(s"wc -l ${file.getAbsolutePath}").!!.split("\\s+").toList.map(_.trim).filter(!_.isEmpty).head.toInt
    })
}
