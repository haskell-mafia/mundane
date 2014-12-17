package com.ambiata.mundane
package io

import com.ambiata.mundane.control._
import java.io._
import scalaz._, effect._, Effect._

object Files {
  def read(path: LocalFile, encoding: String = "UTF-8"): RIO[String] =
    RIO.using(path.toInputStream) { in =>
      Streams.read(in, encoding) }

  def readLines(path: LocalFile, encoding: String = "UTF-8"): RIO[Vector[String]] =
    read(path, encoding).map(_.lines.toVector)

  def write(path: LocalFile, content: String, encoding: String = "UTF-8"): RIO[Unit] = for {
    _ <- Directories.mkdirs(path.dirname)
    _ <- RIO.using(path.toOutputStream) { out =>
      Streams.write(out, content, encoding) }
  } yield ()

  def readBytes(path: LocalFile): RIO[Array[Byte]] =
    RIO.using(path.toInputStream)(Streams.readBytes(_))

  def writeLines(path: LocalFile, content: Seq[String], encoding: String = "UTF-8"): RIO[Unit] = for {
    _ <- Directories.mkdirs(path.dirname)
    _ <- RIO.using(path.toOutputStream)(Streams.write(_, if (content.isEmpty) "" else content.mkString("", "\n", "\n"), encoding))
  } yield ()

  def writeBytes(path: LocalFile, content: Array[Byte]): RIO[Unit] = for {
    _ <- Directories.mkdirs(path.dirname)
    _ <- RIO.using(path.toOutputStream)(Streams.writeBytes(_, content))
  } yield ()

  def writeStream(path: LocalFile, content: InputStream): RIO[Unit] =  for {
    _ <- Directories.mkdirs(path.dirname)
    _ <- RIO.using(path.toOutputStream)(Streams.pipe(content, _))
  } yield ()

  def move(src: LocalFile, dest: LocalFile): RIO[Unit] = RIO.safe {
    val destFile = dest.toFile
    dest.dirname.toFile.mkdirs
    src.toFile.renameTo(destFile)
  }

  def moveTo(src: LocalFile, dest: DirPath): ResultT[IO, Unit] =
    src.basename match {
      case None =>
        ResultT.fail("Source is a top level directory, can't move.")
      case Some(filename) =>
        move(src, dest </ filename)
    }

  def copyTo(src: LocalFile, dest: DirPath): ResultT[IO, Unit] =
    src.basename match {
      case None =>
        ResultT.fail("Source is a top level directory, can't copy.")
      case Some(filename) =>
        val srcFile = src.toFile
        val destLocalFile = dest </ filename
        dest.toFile.mkdirs
        ResultT.using(ResultT.safe[IO, InputStream](new FileInputStream(srcFile)))(writeStream(destLocalFile, _))
    }

  def copy(src: LocalFile, dest: LocalFile): RIO[Unit] = {
    val srcFile = src.toFile
    RIO.using(RIO.safe[InputStream](new FileInputStream(srcFile)))(writeStream(dest, _))
  }

  def exists(filePath: LocalFile): RIO[Boolean] = RIO.safe[Boolean] {
    val file = filePath.toFile
    file.exists && file.isFile
  }

  def delete(path: LocalFile): RIO[Unit] = for {
    e <- exists(path)
  } yield !e || path.toFile.delete
}
