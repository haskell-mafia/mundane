package com.ambiata.mundane
package io

import com.ambiata.mundane.control._
import java.io.{File, FileOutputStream}

import scala.sys.process._

import scalaz._, Scalaz._, \&/._
import scalaz.effect._

case class Archive(archiveFile: File, checksumFile: File, contents: List[File], checksum: Checksum) {
  def files: List[File] =
    List(archiveFile, checksumFile)
}

object Archive {
  // FIX This is the original copper publish code, for creating archives, but it has some ill-defined sematics
  //     around printing errors to standard out as well as some very  _optimistic_ quoting. Needs work.
  def create(archiveFilename: String, md5Filename: String, targetDirectory: File, contents: List[File]): ResultT[IO, Archive] = {
    val targetFilename = s"${targetDirectory.getAbsolutePath}/${archiveFilename}"
    val targetFile = new File(targetFilename)
    val md5File = new File(s"${targetDirectory.getAbsolutePath}/${md5Filename}")
    val srcFiles = contents.map(_.getName).mkString(" ")
    val command = List("sh", "-c", s"tar czf ${targetFilename} -C ${targetDirectory.getAbsolutePath} ${srcFiles} > /dev/null")

    ResultT.safe[IO, Int] { Process(command) ! ProcessLogger(o => (), println) }.flatMap({
      case 0 => for {
        checksum <- Checksum.file(targetFile, MD5).liftIO[ResultTIO]
        _        <- IO { Streams.write(new FileOutputStream(md5File), checksum.hash) }.liftIO[ResultTIO]
      } yield Archive(targetFile, md5File, contents, checksum)
      case e =>
        ResultT.fail[IO, Archive](s"Error compressing files, tar exit code <$e>")
    })
  }
}
