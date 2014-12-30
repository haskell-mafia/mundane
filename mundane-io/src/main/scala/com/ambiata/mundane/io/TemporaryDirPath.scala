package com.ambiata.mundane.io

import com.ambiata.mundane.control._
import com.ambiata.mundane.io.Temporary._

import scalaz._, Scalaz._, effect._

case class TemporaryDirPath(dir: DirPath) {
  def clean: RIO[Boolean] =
    Directories.delete(dir)
}

object TemporaryDirPath {
  implicit val TemporaryDirPathResource = new Resource[TemporaryDirPath] {
    def close(temp: TemporaryDirPath) = temp.clean.run.void // Squelch errors
  }

  def withDirPath[A](f: DirPath => RIO[A]): RIO[A] = {
    val dir = uniqueDirPath
    Directories.mkdirs(dir) >> runWithDirPath(dir)(f)
  }

  def runWithDirPath[A](dir: DirPath)(f: DirPath => RIO[A]): RIO[A] =
    RIO.using(RIO.ok(TemporaryDirPath(dir)))(tmp => f(tmp.dir))
}
