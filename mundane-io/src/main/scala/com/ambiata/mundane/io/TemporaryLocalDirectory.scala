package com.ambiata.mundane.io

import com.ambiata.mundane.control._
import com.ambiata.mundane.io.Temporary._

import scalaz._, Scalaz._, effect._

case class TemporaryLocalDirectory(dir: LocalDirectory) {
  def clean: RIO[Unit] =
    dir.delete
}

object TemporaryLocalDirectory {
  implicit val TemporaryLocalDirectoryResource = new Resource[TemporaryLocalDirectory] {
    def close(temp: TemporaryLocalDirectory) = temp.clean.run.void // Squelch errors
  }

  def withLocalDirectory[A](f: LocalDirectory => RIO[A]): RIO[A] =
    LocalPath(uniqueLocalPath).mkdirs >>= (d => runWithLocalDirectory(d)(f))

  def runWithLocalDirectory[A](dir: LocalDirectory)(f: LocalDirectory => RIO[A]): RIO[A] =
    RIO.using(TemporaryLocalDirectory(dir).pure[RIO])(tmp => f(tmp.dir))
}
