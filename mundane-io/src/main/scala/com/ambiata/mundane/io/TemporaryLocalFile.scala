package com.ambiata.mundane.io

import com.ambiata.mundane.control._
import com.ambiata.mundane.io.Temporary._

import scalaz._, Scalaz._, effect._

case class TemporaryLocalFile(file: LocalFile) {
  def clean: RIO[Unit] =
    file.delete
}

object TemporaryLocalFile {
  implicit val TemporaryLocalFileResource = new Resource[TemporaryLocalFile] {
    def close(temp: TemporaryLocalFile) = temp.clean.run.void // Squelch errors
  }

  def withLocalFile[A](f: LocalFile => RIO[A]): RIO[A] = ???
//    runWithLocalFile(uniqueLocalFile)(f)

  def runWithLocalFile[A](file: LocalFile)(f: LocalFile => RIO[A]): RIO[A] =
    RIO.using[TemporaryLocalFile, TemporaryLocalFile, A](TemporaryLocalFile(file).pure[RIO])(tmp => f(tmp.file))
}
