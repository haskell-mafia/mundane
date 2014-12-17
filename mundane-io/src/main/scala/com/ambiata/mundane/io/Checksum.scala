package com.ambiata.mundane
package io

import com.ambiata.mundane.control._

import java.io._
import java.security.MessageDigest

import scalaz._, Scalaz._, \&/._
import scalaz.effect._, Effect._

case class Checksum(algorithm: ChecksumAlgorithm, hash: String)

sealed abstract class ChecksumAlgorithm(val value: String)
case object MD5 extends ChecksumAlgorithm("MD5")
case object SHA1 extends ChecksumAlgorithm("SHA1")

object Checksum {
  def toHexString(bytes: Array[Byte]): String =
    bytes.map("%02X".format(_)).mkString.toLowerCase

  def stream(is: InputStream, algorithm: ChecksumAlgorithm, bufferSize: Int = 4096): RIO[Checksum] =
    RIO.safe[Checksum]  { unsafe(is, algorithm, bufferSize) }

  def file(f: LocalFile, algorithm: ChecksumAlgorithm): RIO[Checksum] =
    RIO.using(f.toInputStream) { in =>
      stream(in, algorithm)
    }

  def string(s: String, algorithm: ChecksumAlgorithm): Checksum =
    bytes(s.getBytes("UTF-8"), algorithm)

  def bytes(bs: Array[Byte], algorithm: ChecksumAlgorithm): Checksum =
    unsafe(new ByteArrayInputStream(bs), algorithm)

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
