package com.ambiata.mundane
package io

import com.ambiata.mundane.control._

import org.joda.time.format.DateTimeFormat

import scalaz._, Scalaz._
import scalaz.effect._

case class Temporary(dir: DirPath) {
  def clean: ResultT[IO, Boolean] =
    Directories.delete(dir)
}

object Temporary {
  implicit val TemporaryResource = new Resource[Temporary] {
    def close(temp: Temporary) = temp.clean.run.void // Squelch errors
  }

  def directory(base: DirPath, prefix: String): ResultT[IO, Temporary] = {
    val formatter = DateTimeFormat.forPattern("yyyyMMddHHmmss")
    val now = System.currentTimeMillis
    val seed = scala.util.Random.nextInt
    val t = base </> FileName.unsafe(s"$prefix-${formatter.print(now)}-$seed")
    Directories.mkdirs(t).as(Temporary(t))
  }

  def using[A](f: DirPath => ResultTIO[A]): ResultTIO[A] =
    ResultT.using(createTempDir)(temp => f(temp.dir))

  def createTempDir =
    directory(DirPath.unsafe(System.getProperty("java.io.tmpdir", "/tmp")), "temporary")
}
