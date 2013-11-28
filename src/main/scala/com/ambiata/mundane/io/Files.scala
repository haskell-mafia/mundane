package com.ambiata.mundane
package io

import java.io._
import java.security.MessageDigest
import scalaz._, Scalaz._

object Files {
  def calcChecksum(f: File): String \/ String =
    checksumFile(f).map(toHexString)

  def toHexString(bytes: Array[Byte]): String =
    bytes.map("%02X".format(_)).mkString.toLowerCase

  def unsafeChecksum(is: InputStream): Array[Byte] = {
    val buffer = Array.ofDim[Byte](4096)
    var length = 0
    val digester = MessageDigest.getInstance("MD5")
    while ({ length = is.read(buffer, 0, buffer.length); length != -1 }) {
      digester.update(buffer, 0, length)
    }
    digester.digest
  }

  def checksumFile(f: File): String \/ Array[Byte] =  {
    val fis = new FileInputStream(f)
    try checksumStream(fis) finally fis.close()
  }

  def checksumStream(is: InputStream): String \/ Array[Byte] = 
    try unsafeChecksum(is).right catch { case t: Throwable => t.getMessage.left }

  def readFile(f: File): String \/ String =
    readFileBytes(f).map(new String(_))

  def readFileBytes(f: File): String \/ Array[Byte] =
    try org.apache.commons.io.FileUtils.readFileToByteArray(f).right catch { case t: Throwable => t.getMessage.left }

  def writeInputStream(is: InputStream, os: OutputStream): String \/ OutputStream =
    try { org.apache.commons.io.IOUtils.copy(is, os); os.right } catch { case t: Throwable => t.getMessage.left }

  def writeInputStreamToFile(is: InputStream, f: File, mkdirs: Boolean = false): String \/ File = {
    val p = Option(f.getParentFile)
    for {
      _ <- if(mkdirs && p.isDefined) p.map(mkdir).get else ().right
      os = new FileOutputStream(f)
      _ <- (try writeInputStream(is, os) finally os.close())
    } yield f
  }

  def mkdir(d: File): String \/ File =
    if(d.exists) if(d.isFile) s"$d is a file which exists!".left else d.right
    else if(d.mkdirs()) d.right
    else s"Could not create dir $d!".left

  def printToFile(f: File)(op: java.io.PrintWriter => Unit): String \/ File = {
    val p = new java.io.PrintWriter(f)
    try { op(p) }
    catch { case e: Exception => "Could not write to file '${f.getPath}' - '${e.getMessage}".left }
    finally { p.close() }
    f.right
  }

  def mv(src: File, dest: File): String \/ File =
    try
      if(src.renameTo(dest)) dest.right else s"Could not move ${src.getPath} to ${dest.getPath}".left
    catch {
      case t: Throwable => t.getMessage.left
    }

  def validGzip(f: File): Boolean =
    !gzipError(f).isDefined

  def gzipError(f: File): Option[String] = {
    import scala.sys.process._
    val sw = new StringWriter
    if(f.length == 0)
      Some(s"${f.getAbsolutePath} is empty")
    else if((List("sh", "-c", s"gzip -dc ${f.getPath} > /dev/null") ! ProcessLogger(o => (), e => sw.write(s"${e}\n"))) != 0)
      Some(sw.toString)
    else
      None
  }

  def validTarball(f: File): Boolean =
    !tarballError(f).isDefined

  def tarballError(f: File): Option[String] = {
    import scala.sys.process._
    val sw = new StringWriter
    if(f.length == 0)
      Some(s"${f.getAbsolutePath} is empty")
    else if((List("sh", "-c", s"tar xfz ${f.getPath} -O > /dev/null") ! ProcessLogger(o => (), e => sw.write(s"${e}\n"))) != 0)
      Some(sw.toString)
    else
      None
  }

  def extractTarballStream(tarball: InputStream, destDir: File, stripLevels: Int = 0): String \/ File = {
    import scala.sys.process._
    val cmd = s"tar xz -C ${destDir.getPath} -" + (if(stripLevels > 0) s"-strip-components ${stripLevels} -" else "")
    val sw = new StringWriter
    if(!destDir.exists && !destDir.mkdirs())
      s"Could not create tarball extraction dir ${destDir}!".left
    else if(!destDir.isDirectory)
      s"${destDir} is not a directory!".left
    else if((List("sh", "-c", cmd) #< tarball ! ProcessLogger(o => (), e => sw.write(s"${e}\n"))) != 0)
      s"Could not extract tarball, stderr - ${sw.toString}".left
    else
      destDir.right
  }
}
