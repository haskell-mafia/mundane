package com.ambiata.mundane
package io

import java.io._
import org.specs2._
import matcher._
import java.security.MessageDigest

class FilesSpec extends Specification with ScalaCheck with ContentMatchers { def is = s2"""

Files should be able to:
  read bytes from file                        $e1
  read string from file                       $e2
  read utf8 string with new line from file    $e3
  calculate checksum of a file                $e4
  validate tarball                            $e5
  validate gzip                               $e6
  extract a tarball from a stream             $e7
"""

  def e1 = prop((bs: Array[Byte]) => {
    val tmpFile = File.createTempFile("files-spec", ".bytes")
    writeBytes(tmpFile, bs)
    val fileBytes = Files.readFileBytes(tmpFile)
    tmpFile.delete()
    fileBytes.toOption.get === bs
  })

  def e2 = prop((str: String) => canReadFile(str)).set(minTestsOk = 1000)
  def e3 = canReadFile("""섋騚㊼乡왇㛩鴄〫⑁䨜嵏风녇佞ው煓괄ꎮꒀ醆魓ﰺ評떜뻀썲荘㳰锉䤲߶㊢ᅫ㠏⴫⃅⒊逢墵⍓刹军""")

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

  def e7 = prop((bs: Array[Byte], str: String) => {
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
  })

  def mkTempDir(prefix: String, suffix: String = System.nanoTime.toString): File = {
    val tmpFile = File.createTempFile(prefix, suffix)
    if(!tmpFile.delete()) sys.error(s"Could not delete temp file '${tmpFile.getAbsolutePath}'")
    if(!tmpFile.mkdir()) sys.error(s"Could not create temp dir '${tmpFile.getAbsolutePath}'")
    tmpFile
  }

  def tarball(inDir: File, outFile: File): File = {
    import scala.sys.process._
    Process(List("sh", "-c", s"tar czf ${outFile.getPath} --directory ${inDir.getPath} .")).run(ProcessLogger(stdout => (), println))
    outFile
  }

  def gzip(file: File): File = {
    import scala.sys.process._
    val gzfile = new File(file.getPath + ".gz")
    Process(List("sh", "-c", s"gzip -c ${file.getPath} > ${gzfile.getPath}")).run(ProcessLogger(o => (), println))
    gzfile
  }

  def writeBytes(f: File, bytes: Array[Byte]) {
    val fos =  new FileOutputStream(f)
    fos.write(bytes)
    fos.close()
  }

  def writeString(f: File, str: String) {
    val pw = new PrintWriter(f)
    pw.write(str)
    pw.close()
  }

  def rmdir(d: File) {
    if(d.isDirectory) d.listFiles.foreach(rmdir) else d.delete
    d.delete
  }
}
