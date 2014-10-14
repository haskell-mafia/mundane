package com.ambiata.mundane.io

import com.ambiata.mundane.control._
import com.ambiata.mundane.io.Temporary._

import scalaz._, Scalaz._, effect._

case class TemporaryDirPath(dir: DirPath) {
  def clean: ResultT[IO, Boolean] =
    Directories.delete(dir)
}

object TemporaryDirPath {
  implicit val TemporaryDirPathResource = new Resource[TemporaryDirPath] {
    def close(temp: TemporaryDirPath) = temp.clean.run.void // Squelch errors
  }

  def withDirPath[A](f: DirPath => ResultTIO[A]): ResultTIO[A] = {
    val dir = uniqueDirPath
    Directories.mkdirs(dir) >> runWithDirPath(dir)(f)
  }

  def runWithDirPath[A](dir: DirPath)(f: DirPath => ResultTIO[A]): ResultTIO[A] =
    ResultT.using(TemporaryDirPath(dir).pure[ResultTIO])(tmp => f(tmp.dir))
}
