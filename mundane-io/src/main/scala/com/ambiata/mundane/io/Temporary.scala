package com.ambiata.mundane
package io

import com.ambiata.mundane.control._

import org.joda.time.format.DateTimeFormat

import scalaz._, Scalaz._
import scalaz.effect._

case class Temporary(file: FilePath) {
  def clean: ResultT[IO, Unit] =
    Directories.delete(file)
}

object Temporary {
  implicit val TemporaryResource = new Resource[Temporary] {
    def close(temp: Temporary) = temp.clean.run.void // Squelch errors
  }

  def directory(base: FilePath, prefix: String): ResultT[IO, Temporary] = {
    val formatter = DateTimeFormat.forPattern("yyyyMMddHHmmss")
    val now = System.currentTimeMillis
    val seed = scala.util.Random.nextInt
    val t = base </> s"${prefix}-${formatter.print(now)}-${seed}"
    Directories.mkdirs(t).as(Temporary(t))
  }

  def using[A](f: FilePath => ResultTIO[A]): ResultTIO[A] =
    ResultT.using(directory(FilePath(System.getProperty("java.io.tmpdir", "/tmp")), "temporary"))(temp => f(temp.file))
}
