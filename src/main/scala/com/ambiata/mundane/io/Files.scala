package com.ambiata.mundane
package io

import java.io.File
import java.security.MessageDigest
import scalaz._, Scalaz._

object Files {
  def calcChecksum(f: File): String \/ String =
    readFileBytes(f).map(md5Hex)

  def md5Hex(bytes: Array[Byte]): String =
    md5Bytes(bytes).map("%02X".format(_)).mkString.toLowerCase

  def md5Bytes(bytes: Array[Byte]): Array[Byte] =
    MessageDigest.getInstance("MD5").digest(bytes)

  def readFileBytes(f: File): String \/ Array[Byte] =
    try org.apache.commons.io.FileUtils.readFileToByteArray(f).right catch { case t: Throwable => t.getMessage.left }
}
