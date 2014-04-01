package com.ambiata.mundane
package io

import com.ambiata.mundane.control._

import java.io.File

import scalaz._, Scalaz._
import scalaz.effect._

object Directories {
  def mkdirs(path: FilePath): ResultT[IO, Unit] =
    ResultT.safe[IO, Boolean] { path.toFile.mkdirs } void

  def list(path: FilePath): ResultT[IO, List[FilePath]] = ResultT.safe[IO, List[FilePath]] {
    def loop(file: File): List[File] =
      if (file.isDirectory)
        Option(file.listFiles).cata(_.toList, Nil).flatMap(loop)
      else if (file.exists)
        List(file)
      else
        List()
    loop(path.toFile).map(f => FilePath(f.getPath))
  }

  def delete(path: FilePath): ResultT[IO, Unit] = ResultT.safe[IO, Unit] {
    def loop(file: File): Unit = {
      if (file.isDirectory)
        Option(file.listFiles).cata(_.toList, Nil).foreach(loop)
      file.delete
      ()
    }
    loop(path.toFile)
  }

  def exists(path: FilePath): ResultT[IO, Boolean] = ResultT.safe[IO, Boolean] {
    val file = path.toFile
    file.exists && file.isDirectory
  }
}
