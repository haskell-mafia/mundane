package com.ambiata.mundane
package io

import com.ambiata.mundane.control._
import java.io._
import scalaz._, effect._, Effect._

object Files {
  def read(path: FilePath, encoding: String = "UTF-8"): ResultT[IO, String] =
    ResultT.using(path.toInputStream) { in =>
      Streams.read(in, encoding) }

  def readLines(path: FilePath, encoding: String = "UTF-8"): ResultT[IO, Vector[String]] =
    read(path, encoding).map(_.split("\n").toVector)

  def write(path: FilePath, content: String, encoding: String = "UTF-8"): ResultT[IO, Unit] = for {
    _ <- Directories.mkdirs(path.dirname)
    _ <- ResultT.using(path.toOutputStream) { out =>
      Streams.write(out, content, encoding) }
  } yield ()

  def readBytes(path: FilePath): ResultT[IO, Array[Byte]] =
    ResultT.using(path.toInputStream)(Streams.readBytes(_))

  def writeLines(path: FilePath, content: Seq[String], encoding: String = "UTF-8"): ResultT[IO, Unit] = for {
    _ <- Directories.mkdirs(path.dirname)
    _ <- ResultT.using(path.toOutputStream)(Streams.write(_, if (content.isEmpty) "" else content.mkString("", "\n", "\n"), encoding))
  } yield ()

  def writeBytes(path: FilePath, content: Array[Byte]): ResultT[IO, Unit] = for {
    _ <- Directories.mkdirs(path.dirname)
    _ <- ResultT.using(path.toOutputStream)(Streams.writeBytes(_, content))
  } yield ()

  def writeStream(path: FilePath, content: InputStream): ResultT[IO, Unit] =  for {
    _ <- Directories.mkdirs(path.dirname)
    _ <- ResultT.using(path.toOutputStream)(Streams.pipe(content, _))
  } yield ()

  def move(src: FilePath, dest: FilePath): ResultT[IO, Unit] = ResultT.safe {
    val destFile = dest.toFile
    dest.dirname.toFile.mkdirs
    src.toFile.renameTo(destFile)
  }

  def move(src: FilePath, dest: DirPath): ResultT[IO, Unit] =
    move(src, dest <|> src.basename)

  def copy(src: FilePath, dest: DirPath): ResultT[IO, Unit] = {
    val srcFile = src.toFile
    val destFilePath = dest <|> src.basename
    dest.toFile.mkdirs

    ResultT.using(ResultT.safe[IO, InputStream](new FileInputStream(srcFile)))(writeStream(destFilePath, _))
  }

  def copy(src: FilePath, dest: FilePath): ResultT[IO, Unit] = {
    val srcFile = src.toFile
    ResultT.using(ResultT.safe[IO, InputStream](new FileInputStream(srcFile)))(writeStream(dest, _))
  }

  def exists(filePath: FilePath): ResultT[IO, Boolean] = ResultT.safe[IO, Boolean] {
    val file = filePath.toFile
    file.exists && file.isFile
  }

  def delete(path: FilePath): ResultT[IO, Unit] = for {
    e <- exists(path)
  } yield !e || path.toFile.delete
}
