package com.ambiata.mundane
package io

import java.io._
import scalaz._, Scalaz._
import scalaz.effect._

object Files {
  def calcChecksum(f: File): String \/ String =
    Checksum.file(f, MD5).map(_.hash.right[String]).except(t => IO { t.getMessage.left[String] }).unsafePerformIO

  def readFile(f: File, encoding: String = "UTF-8"): String \/ String =
    readFileBytes(f).map(new String(_, encoding))

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

  def printToFile(f: File, encoding: String = "UTF-8")(op: java.io.PrintStream => Unit): String \/ File = {
    val fos = new FileOutputStream(f)
    val p = new java.io.PrintStream(fos, false, encoding)
    try { op(p) }
    catch { case e: Exception => "Could not write to file '${f.getPath}' - '${e.getMessage}".left }
    finally { p.close(); fos.close() }
    f.right
  }

  def printToFiles[A](fs: List[(A, File)], encoding: String = "UTF-8")(op: (A => java.io.PrintStream) => Unit): String \/ List[(A, File)] = {
    val foss = fs .map { case (a, f) => (a, new FileOutputStream(f)) }
    val ps = foss .map { case (a, fos) => (a, new java.io.PrintStream(fos, false, encoding)) } .toMap
    try { op(ps) }
    catch { case e: Exception => s"Could not write to file - '${e.getMessage}".left }
    finally {
      ps.values.foreach(_.close())
      foss.map(_._2).foreach(_.close())
    }
    fs.right
  }

  def mv(src: File, dest: File): String \/ File =
    try
      if(src.renameTo(dest)) dest.right else s"Could not move ${src.getPath} to ${dest.getPath}".left
    catch {
      case t: Throwable => t.getMessage.left
    }

  def ls(dir: File): String \/ List[File] =
    if(!dir.exists)
      s"Directory ${dir.getPath} does not exist!".left
    else if(!dir.isDirectory)
      s"${dir.getPath} is not a directory!".left
    else
      try
        Option(dir.listFiles).map(_.toList.right).getOrElse(s"Could not list files under ${dir.getPath}".left)
      catch {
        case t: Throwable => s"Got exception when trying to list files under ${dir.getPath} - ${t.getMessage}".left
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

  /**
   * Always writes to the file "output" then returns the path to it.
   * @param gzip
   * @param destDir
   * @return
   */
  def extractGzipStream(gzip: InputStream, destFile: File): String \/ File = {
    import scala.sys.process._
    val cmd = s"gzip -dc -"
    val sw = new StringWriter
    if(!destFile.getParentFile.exists && !destFile.getParentFile.mkdirs())
      s"Could not create gzip extraction dir ${destFile.getParent}!".left
    else if(destFile.isDirectory)
      s"${destFile} is a directory!".left
    else if((List("sh", "-c", cmd) #< gzip #> destFile ! ProcessLogger(o => (), e => sw.write(s"${e}\n"))) != 0)
      s"Could not extract gzip, stderr - ${sw.toString}".left
    else
      destFile.right
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

    val cmd = s"tar xz -C ${destDir.getPath} -" + (if(stripLevels > 0) s"-strip-components ${stripLevels}" else "")
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
