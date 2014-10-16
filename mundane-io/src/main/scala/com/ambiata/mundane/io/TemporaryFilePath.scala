package com.ambiata.mundane.io

import com.ambiata.mundane.control._
import com.ambiata.mundane.io.Temporary._

import scalaz._, Scalaz._, effect._

case class TemporaryFilePath(file: FilePath) {
  def clean: ResultT[IO, Unit] =
    Files.delete(file)
}

object TemporaryFilePath {
  implicit val TemporaryFilePathResource = new Resource[TemporaryFilePath] {
    def close(temp: TemporaryFilePath) = temp.clean.run.void // Squelch errors
  }

  def withFilePath[A](f: FilePath => ResultTIO[A]): ResultTIO[A] =
    runWithFilePath(uniqueFilePath)(f)

  def runWithFilePath[A](file: FilePath)(f: FilePath => ResultTIO[A]): ResultTIO[A] =
    ResultT.using[TemporaryFilePath, TemporaryFilePath, A](TemporaryFilePath(file).pure[ResultTIO])(tmp => f(tmp.file))
}
