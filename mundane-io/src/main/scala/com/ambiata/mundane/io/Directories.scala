package com.ambiata.mundane
package io

import com.ambiata.mundane.control._

import java.io.{FileInputStream, InputStream, File}

import scalaz._, Scalaz._
import scalaz.effect._

object Directories {
  def mkdirs(dirPath: DirPath): ResultT[IO, Unit] =
    ResultT.safe[IO, Boolean](dirPath.toFile.mkdirs).void

  def list(dirPath: DirPath): ResultT[IO, List[FilePath]] = ResultT.safe[IO, List[FilePath]] {
    def loop(dir: DirPath): Vector[FilePath] = {
      val files = Option(dir.toFile.listFiles).cata(_.toVector, Vector())
      files.flatMap { f =>
        if (f.isDirectory) loop(dir </> FileName.unsafe(f.getName))
        else               Vector(FilePath.unsafe(f))
      }
    }
    loop(dirPath).toList
  }


  def delete(dirPath: DirPath): ResultT[IO, Boolean] = ResultT.safe[IO, Boolean] {
    def loop(dir: DirPath): Boolean = {
      val files = Option(dir.toFile.listFiles).cata(_.toVector, Vector())
      files.forall { f =>
        if (f.isDirectory) loop(dir </> FileName.unsafe(f.getName))
        else               f.delete
      } && dir.toFile.delete
    }
    loop(dirPath)
  }

  def move(src: DirPath, dest: DirPath): ResultT[IO, Unit] = ResultT.safe {
    val destf = dest.toFile
    destf.mkdirs
    src.toFile.renameTo(destf)
  }

  def exists(dirPath: DirPath): ResultT[IO, Boolean] = ResultT.safe[IO, Boolean] {
    val file = dirPath.toFile
    file.exists && file.isDirectory
  }

}