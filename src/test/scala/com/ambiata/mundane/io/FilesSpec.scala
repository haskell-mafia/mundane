package com.ambiata.mundane
package io

import java.io._
import org.specs2._
import java.security.MessageDigest

class FilesSpec extends Specification with ScalaCheck { def is = s2"""

Files should be able to:
  read bytes from file                        $read
  calculate checksums of a file               $calcChecksum

"""

  def read = prop((bs: Array[Byte]) => {
    val tmpFile = File.createTempFile("files-spec", ".bytes")
    val fos = new FileOutputStream(tmpFile)
    fos.write(bs)
    val fileBytes = Files.readFileBytes(tmpFile)
    tmpFile.delete
    fileBytes.toOption.get === bs
  })

  def calcChecksum = prop((bs: Array[Byte]) => {
    val tmpFile = File.createTempFile("files-calc-checksum-spec", ".bytes")
    val fos = new FileOutputStream(tmpFile)
    fos.write(bs)
    val checksum = Files.calcChecksum(tmpFile)
    tmpFile.delete

    val expectedChecksum = MessageDigest.getInstance("MD5").digest(bs).map("%02X".format(_)).mkString.toLowerCase

    checksum.toOption === Some(expectedChecksum)

  })
}
