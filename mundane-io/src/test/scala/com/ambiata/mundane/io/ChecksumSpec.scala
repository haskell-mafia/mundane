package com.ambiata.mundane
package io

import com.ambiata.mundane.testing.ResultTIOMatcher._

import org.specs2._
import org.scalacheck._, Arbitrary._

import scalaz._, Scalaz._

class ChecksumSpec extends Specification with ScalaCheck { def is = s2"""

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


  def md5 =
    Checksum.string("hello", MD5).hash must_== "5d41402abc4b2a76b9719d911017c592"

  def sha1 =
    Checksum.string("hello", SHA1).hash must_== "aaf4c61ddcc5e8a2dabede0f3b482cd9aea9434d"

  def algorithm(alg: ChecksumAlgorithm) =
    prop((s: String) =>
      Checksum.string(s, alg).algorithm must_== alg)

  def text = prop((s: String) => TemporaryDirPath.withDirPath { work =>
    val path = work </ "text"
    Files.write(path, s) >> Checksum.file(path, MD5)
  } must beOkValue(Checksum.string(s, MD5)))

  def bytes = prop((b: Array[Byte]) => TemporaryDirPath.withDirPath { work =>
    val path = work </ "bytes"
    Files.writeBytes(path, b) >> Checksum.file(path, MD5)
  } must beOkValue(Checksum.bytes(b, MD5)))
}
