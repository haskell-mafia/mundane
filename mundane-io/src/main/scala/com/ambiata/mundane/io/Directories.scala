package com.ambiata.mundane
package io

import java.io.File

import scalaz._, Scalaz._
import scalaz.effect._

object Directories {
  def mkdirs(file: File): IO[Unit] = IO {
    file.mkdirs
    ()
  }

  def list(file: File): IO[List[File]] = IO {
    def loop(file: File): List[File] =
      if (file.isDirectory)
        Option(file.listFiles).cata(_.toList, Nil).flatMap(loop)
      else
        List(file)
    loop(file)
  }

  def delete(file: File): IO[Unit] = IO {
    def loop(file: File): Unit = {
      if (file.isDirectory)
        Option(file.listFiles).cata(_.toList, Nil).foreach(loop)
      file.delete
      ()
    }
    loop(file)
  }

  def exists(file: File): IO[Boolean] = IO {
    file.exists && file.isDirectory
  }
}
