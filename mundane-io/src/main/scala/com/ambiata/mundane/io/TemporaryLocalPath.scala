package com.ambiata.mundane.io

import com.ambiata.mundane.control._
import com.ambiata.mundane.path._
import com.ambiata.mundane.io.Temporary._

import scalaz._, Scalaz._, effect._

case class TemporaryLocalPath(path: LocalPath) {
  def clean: RIO[Unit] =
    path.deleteAll
}

object TemporaryLocalPath {
  implicit val TemporaryLocalPathResource = new Resource[TemporaryLocalPath] {
    def close(temp: TemporaryLocalPath) = temp.clean.run.void // Squelch errors
  }

  def withLocalPath[A](f: LocalPath => RIO[A]): RIO[A] = {
    runWithLocalPath(LocalPath(uniqueLocalPath))(f)
  }

  def runWithLocalPath[A](path: LocalPath)(f: LocalPath => RIO[A]): RIO[A] =
    RIO.using[TemporaryLocalPath, TemporaryLocalPath, A](TemporaryLocalPath(path).pure[RIO])(tmp => f(tmp.path))
}
