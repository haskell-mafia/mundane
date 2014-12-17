package com.ambiata.mundane.io

import com.ambiata.mundane.control._
import com.ambiata.mundane.io.Temporary._

import scalaz._, Scalaz._, effect._

case class TemporaryLocalDirectory(dir: LocalDirectory) {
  def clean: ResultT[IO, Boolean] =
    Directories.delete(dir)
}

object TemporaryLocalDirectory {
  implicit val TemporaryLocalDirectoryResource = new Resource[TemporaryLocalDirectory] {
    def close(temp: TemporaryLocalDirectory) = temp.clean.run.void // Squelch errors
  }

  def withLocalDirectory[A](f: LocalDirectory => RIO[A]): RIO[A] = {
    val dir = uniqueLocalDirectory
    Directories.mkdirs(dir) >> runWithLocalDirectory(dir)(f)
  }

  def runWithLocalDirectory[A](dir: LocalDirectory)(f: LocalDirectory => RIO[A]): RIO[A] =
    ResultT.using(TemporaryLocalDirectory(dir).pure[RIO])(tmp => f(tmp.dir))
}
