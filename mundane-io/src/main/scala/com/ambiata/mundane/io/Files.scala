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
    else
      NFiles.move(srcf.toPath, destf.toPath, REPLACE_EXISTING)
  }
}
