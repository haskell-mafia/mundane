package com.ambiata
package mundane
package io

import java.io._
import org.specs2._
import matcher._
import java.security.MessageDigest
import scrutiny.files._

class FilesSpec extends Specification with ScalaCheck with ContentMatchers with LocalFiles {
  
def is = isolated ^ s2"""

Files should be able to:
  read bytes from file                        $e1
  read string from file                       $e2
  read utf8 string with new line from file    $e3
  calculate checksum of a file                $e4
  validate tarball                            $e5
  validate gzip                               $e6
  extract a tarball from a stream             $e7
  list files in a dir                         $e8
"""

  def e1 = prop((bs: Array[Byte]) => {
    val tmpFile = File.createTempFile("files-spec", ".bytes")
    writeBytes(tmpFile, bs)
    val fileBytes = Files.readFileBytes(tmpFile)
    tmpFile.delete()
    fileBytes.toOption.get === bs
  })

  def e2 = prop((str: String) => canReadFile(str)).set(minTestsOk = 1000)
  def e3 = canReadFile("""섋騚㊼
乡왇㛩鴄〫⑁䨜嵏风녇佞ው煓괄ꎮꒀ醆魓ﰺ評떜뻀썲荘㳰锉䤲߶㊢ᅫ㠏⴫⃅⒊逢墵⍓刹军""")

  def canReadFile(str: String) = {
    val tmpFile = File.createTempFile("files-spec", ".string")
    writeString(tmpFile, str)
    val fileStr = Files.readFile(tmpFile)
    tmpFile.delete()
    fileStr.toOption.get === str
  }

  def e4 = prop((bs: Array[Byte]) => {
    val tmpFile = File.createTempFile("files-calc-checksum-spec", ".bytes")
    writeBytes(tmpFile, bs)
    val checksum = Files.calcChecksum(tmpFile)
    tmpFile.delete()
    val expectedChecksum = MessageDigest.getInstance("MD5").digest(bs).map("%02X".format(_)).mkString.toLowerCase
    checksum.toOption === Some(expectedChecksum)
  })

  def e5 = prop((bs: Array[Byte], str: String) => {
    val tmpDir = mkTempDir("files-spec")
    val tmpBinFile = File.createTempFile("files-spec", ".bytes", tmpDir)
    val tmpStrFile = File.createTempFile("files-spec", ".string", tmpDir)
    val tmpTgzFile = File.createTempFile("files-spec", ".tar.gz")

    writeBytes(tmpBinFile, bs)
    writeString(tmpStrFile, str)
    tarball(tmpDir, tmpTgzFile)

    Files.validTarball(tmpTgzFile) === true
    Files.tarballError(tmpStrFile) must beSome

    rmdir(tmpDir)
    tmpTgzFile.delete()
  })

  def e6 = prop((str: String) => {
    val tmpStrFile = File.createTempFile("files-spec", ".string")

    writeString(tmpStrFile, str)
    val tmpGzipFile = gzip(tmpStrFile)

    Files.validGzip(tmpGzipFile) === true
    Files.validGzip(tmpStrFile) === false
  })

  def e7 = {
    val bs: Array[Byte] = Array('a', 'b', '\0')
    val str: String = "foostring"
    val outDir = mkTempDir("files-spec_extract")
    val tmpDir = mkTempDir("files-spec")
    val tmpBinFile = File.createTempFile("files-spec", ".bytes", tmpDir)
    val tmpStrFile = File.createTempFile("files-spec", ".string", tmpDir)
    val tmpTgzFile = File.createTempFile("files-spec", ".tar.gz")

    writeBytes(tmpBinFile, bs)
    writeString(tmpStrFile, str)
    tarball(tmpDir, tmpTgzFile)

    val fis = new FileInputStream(tmpTgzFile)
    try Files.extractTarballStream(fis, outDir).toEither must beRight finally fis.close()
    (tmpBinFile, new File(outDir, tmpBinFile.getName)) must haveSameMD5
    (tmpStrFile, new File(outDir, tmpStrFile.getName)) must haveSameMD5
    outDir must haveSameFilesAs(tmpDir).withMatcher(haveSameMD5)

    rmdir(tmpDir)
    rmdir(outDir)
    tmpTgzFile.delete()
  }

  def e8 = {
    val tmpDir = mkTempDir("files-spec")
    val tmpFile1 = File.createTempFile("files-spec", ".first", tmpDir)
    val tmpFile2 = File.createTempFile("files-spec", ".second", tmpDir)
    val tmpFile3 = File.createTempFile("files-spec", ".third", tmpDir)
    val expected = List(tmpFile1, tmpFile2, tmpFile3)

    writeString(tmpFile1, "first")
    writeString(tmpFile2, "second")
    writeString(tmpFile3, "third")

    Files.ls(tmpDir).toEither must beRight { files: List[File] => files must containTheSameElementsAs(expected) }
  }

  val basePath = new File("target/")
}
