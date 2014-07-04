package com.ambiata.mundane
package io

import com.ambiata.mundane.control._
import java.io._
import java.nio.file.{Files => NFiles, DirectoryNotEmptyException, FileAlreadyExistsException}
import java.nio.file.StandardCopyOption._
import scalaz._, Scalaz._
import scalaz.effect._, Effect._

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
    _ <- ResultT.using(path.toOutputStream)(Streams.write(_, content.mkString("\n"), encoding))
  } yield ()

  def writeBytes(path: FilePath, content: Array[Byte]): ResultT[IO, Unit] = for {
    _ <- Directories.mkdirs(path.dirname)
    _ <- ResultT.using(path.toOutputStream)(Streams.writeBytes(_, content))
  } yield ()

  def writeStream(path: FilePath, content: InputStream): ResultT[IO, Unit] =  for {
    _ <- Directories.mkdirs(path.dirname)
    _ <- ResultT.using(path.toOutputStream)(Streams.pipe(content, _))
  } yield ()

  def move(src: FilePath, dest: FilePath): ResultT[IO, Boolean] = ResultT.safe[IO, Boolean] {
    val srcf = src.toFile
    val destf = dest.toFile
    if (destf.isDirectory)
      try { NFiles.move(srcf.toPath, destf.toPath.resolve(srcf.getName)); true }
      catch { case e: DirectoryNotEmptyException => false }
    else {
      destf.getParentFile.mkdirs
      try { NFiles.move(srcf.toPath, destf.toPath); true }
      catch { case e: FileAlreadyExistsException => false }
    }
  }

  def copy(src: FilePath, dest: FilePath): ResultT[IO, Unit] = ResultT.safe[IO, Unit] {
    val srcf = src.toFile
    val destf = dest.toFile
    if (destf.isDirectory)
      NFiles.copy(srcf.toPath, destf.toPath.resolve(srcf.getName), REPLACE_EXISTING)
    else {
      destf.getParentFile.mkdirs
      NFiles.copy(srcf.toPath, destf.toPath, REPLACE_EXISTING)
    }
  }

  def exists(path: FilePath): ResultT[IO, Boolean] = ResultT.safe[IO, Boolean] {
    path.toFile.exists
  }

  def isFile(path: FilePath): ResultT[IO, Boolean] = ResultT.safe[IO, Boolean] {
    path.toFile.isFile
  }

  def delete(path: FilePath): ResultT[IO, Unit] = for {
    e <- exists(path)
    f <- isFile(path)
    _ <- if (e && f) ResultT.safe[IO, Boolean] { path.toFile.delete }
         else if (e) ResultT.fail[IO, Boolean](s"Can't delete <$path>, it is a directory.")
         else        ResultT.ok[IO, Boolean] { true }
  } yield ()
}
