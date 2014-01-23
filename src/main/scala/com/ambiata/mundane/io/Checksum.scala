package com.ambiata.mundane
package io

import java.io._
import java.security.MessageDigest

import scalaz._, Scalaz._, \&/._
import scalaz.effect._

case class Checksum(algorithm: ChecksumAlgorithm, hash: String)

sealed abstract class ChecksumAlgorithm(val value: String)
case object MD5 extends ChecksumAlgorithm("MD5")
case object SHA1 extends ChecksumAlgorithm("SHA1")

object Checksum {
  def toHexString(bytes: Array[Byte]): String =
    bytes.map("%02X".format(_)).mkString.toLowerCase

  def stream(is: InputStream, algorithm: ChecksumAlgorithm, bufferSize: Int = 4096): IO[Checksum] = IO { unsafe(is, algorithm, bufferSize) }

  def file(f: File, algorithm: ChecksumAlgorithm): IO[Checksum] =  {
    val in = new FileInputStream(f)
    try stream(in, algorithm) finally in.close()
  }

  def string(s: String, algorithm: ChecksumAlgorithm): Checksum =
    unsafe(new ByteArrayInputStream(s.getBytes("UTF-8")), algorithm)

  private def unsafe(is: InputStream, algorithm: ChecksumAlgorithm, bufferSize: Int = 4096): Checksum = {
    val buffer = Array.ofDim[Byte](bufferSize)
    var length = 0
    val digester = MessageDigest.getInstance(algorithm.value)
    while ({ length = is.read(buffer, 0, buffer.length); length != -1 }) {
      digester.update(buffer, 0, length)
    }
    Checksum(algorithm, toHexString(digester.digest))
  }
}
