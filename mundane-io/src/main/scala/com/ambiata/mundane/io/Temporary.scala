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
  def directory(base: FilePath, prefix: String): ResultT[IO, Temporary] = {
    val formatter = DateTimeFormat.forPattern("yyyyMMddHHmmss")
    val now = System.currentTimeMillis
    val seed = scala.util.Random.nextInt
    val t = base </> s"${prefix}-${formatter.print(now)}-${seed}"
    Directories.mkdirs(t).as(Temporary(t))
  }
}
