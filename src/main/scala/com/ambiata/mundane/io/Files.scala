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

  def writeInputStreamToFile(is: InputStream, f: File): String \/ File = {
    val os = new FileOutputStream(f)
    (try writeInputStream(is, os) finally os.close()).map(_ => f)
  }

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
}
