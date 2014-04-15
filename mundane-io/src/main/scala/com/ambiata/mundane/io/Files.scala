package com.ambiata.mundane
package io

import com.ambiata.mundane.control._
import java.io._
import java.nio.file.{Files => NFiles}
import java.nio.file.StandardCopyOption._
import scalaz._, Scalaz._
import scalaz.effect._, Effect._

object Files {
  def read(path: FilePath, encoding: String = "UTF-8"): ResultT[IO, String] =
    ResultT.using(path.toInputStream) { in =>
      Streams.read(in, encoding) }

  def write(path: FilePath, content: String, encoding: String = "UTF-8"): ResultT[IO, Unit] = for {
    _ <- Directories.mkdirs(path.dirname)
    _ <- ResultT.using(path.toOutputStream) { out =>
      Streams.write(out, content, encoding) }
  } yield ()

  def readBytes(path: FilePath): ResultT[IO, Array[Byte]] =
    ResultT.using(path.toInputStream)(Streams.readBytes(_))

  def writeBytes(path: FilePath, content: Array[Byte]): ResultT[IO, Unit] =  for {
    _ <- Directories.mkdirs(path.dirname)
    _ <- ResultT.using(path.toOutputStream)(Streams.writeBytes(_, content))
  } yield ()

  def writeStream(path: FilePath, content: InputStream): ResultT[IO, Unit] =  for {
    _ <- Directories.mkdirs(path.dirname)
    _ <- ResultT.using(path.toOutputStream)(Streams.pipe(content, _))
  } yield ()

  def move(src: FilePath, dest: FilePath): ResultT[IO, Unit] = ResultT.safe[IO, Unit] {
    val srcf = src.toFile
    val destf = dest.toFile
    if (destf.isDirectory)
      NFiles.move(srcf.toPath, destf.toPath.resolve(srcf.getName), REPLACE_EXISTING)
    else {
      destf.getParentFile.mkdirs
      NFiles.move(srcf.toPath, destf.toPath, REPLACE_EXISTING)
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
