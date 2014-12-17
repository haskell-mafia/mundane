package com.ambiata.mundane.io

import com.ambiata.mundane.control.RIO
import com.ambiata.mundane.io.Arbitraries._
import com.ambiata.mundane.testing.RIOMatcher._
import com.ambiata.mundane.path._

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

  def text = prop((s: String, local: LocalTemporary) => for {
    p <- local.fileWithContent(s)
    r <- Checksum.file(p, MD5)
  } yield r ==== Checksum.string(s, MD5))

  def bytes = prop((b: Array[Byte], local: LocalTemporary) => for {
    p <- local.file
    _ <- Files.writeBytes(p, b)
    r <- Checksum.file(p, MD5)
  } yield r ==== Checksum.bytes(b, MD5))

}
