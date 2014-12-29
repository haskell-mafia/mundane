package com.ambiata.mundane.io

import com.ambiata.mundane.control._
import com.ambiata.mundane.io.Temporary._

import scalaz._, Scalaz._, effect._

case class TemporaryFilePath(file: FilePath) {
  def clean: RIO[Unit] =
    Files.delete(file)
}

object TemporaryFilePath {
  implicit val TemporaryFilePathResource = new Resource[TemporaryFilePath] {
    def close(temp: TemporaryFilePath) = temp.clean.run.void // Squelch errors
  }

  def withFilePath[A](f: FilePath => RIO[A]): RIO[A] =
    runWithFilePath(uniqueFilePath)(f)

  def runWithFilePath[A](file: FilePath)(f: FilePath => RIO[A]): RIO[A] =
    RIO.using[TemporaryFilePath, TemporaryFilePath, A](RIO.ok(TemporaryFilePath(file)))(tmp => f(tmp.file))
}
