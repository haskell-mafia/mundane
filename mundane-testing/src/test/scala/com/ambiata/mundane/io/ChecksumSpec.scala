package com.ambiata.mundane
package io

import com.ambiata.mundane.testing.ResultTIOMatcher._

import java.io._
import java.util.UUID

import org.specs2._, matcher._, specification._
import org.scalacheck._, Arbitrary._

import scalaz._, Scalaz._

class ChecksumSpec extends Specification with ScalaCheck with AfterExample { def is = isolated ^ s2"""

Checksum Known Answer Tests
---------------------------

  `printf hello | md5`                                    $md5
  `printf hello | sha1`                                   $sha1

Checksum Properties
-------------------

  Algorithm is always maintained (MD5)                    ${algorithm(MD5)}
  Algorithm is always maintained (SHA1)                   ${algorithm(SHA1)}
  Checksum text via file is correct                       $text
  Checksum bytesvia file is correct                       $bytes

"""


  val work = System.getProperty("java.io.tmpdir", "/tmp") </> s"ChecksumSpec.${UUID.randomUUID}"

  def md5 =
    Checksum.string("hello", MD5).hash must_== "5d41402abc4b2a76b9719d911017c592"

  def sha1 =
    Checksum.string("hello", SHA1).hash must_== "aaf4c61ddcc5e8a2dabede0f3b482cd9aea9434d"

  def algorithm(alg: ChecksumAlgorithm) =
    prop((s: String) =>
      Checksum.string(s, alg).algorithm must_== alg)

  def text = prop((s: String) => {
    val path = work </> "text"
    val action = Files.write(path, s) >> Checksum.file(path, MD5)
    action must beOkValue(Checksum.string(s, MD5))
  })

  def bytes = prop((b: Array[Byte]) => {
    val path = work </> "bytes"
    val action = Files.writeBytes(path, b) >> Checksum.file(path, MD5)
    action must beOkValue(Checksum.bytes(b, MD5))
  })

  def after =
    Directories.delete(work).run.unsafePerformIO
}
