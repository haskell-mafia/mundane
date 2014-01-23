package com.ambiata.mundane
package io

import java.io._
import org.specs2._
import org.scalacheck._, Arbitrary._


class ChecksumSpec extends Specification with ScalaCheck { def is = s2"""

Checksum Known Answer Tests
---------------------------

  `printf hello | md5`                                    $md5
  `printf hello | sha1`                                   $sha1

Checksum Properties
-------------------

  Algoithm is always maintained (MD5)                     ${algorithm(MD5)}
  Algoithm is always maintained (SHA1)                    ${algorithm(SHA1)}

"""

  def md5 =
    Checksum.string("hello", MD5).hash must_== "5d41402abc4b2a76b9719d911017c592"

  def sha1 =
    Checksum.string("hello", SHA1).hash must_== "aaf4c61ddcc5e8a2dabede0f3b482cd9aea9434d"

  def algorithm(alg: ChecksumAlgorithm) =
    prop((s: String) =>
      Checksum.string(s, alg).algorithm must_== alg)
}
