package com.ambiata.mundane
package io

import java.io.File

import org.joda.time.format.DateTimeFormat

import scalaz._, Scalaz._
import scalaz.effect._

object Temporary {
  def directory(base: File, prefix: String): IO[File] = IO {
    val formatter = DateTimeFormat.forPattern("yyyyMMddHHmmss")
    val now = System.currentTimeMillis
    val seed = scala.util.Random.nextInt
    val t = new File(base, s"${prefix}-${formatter.print(now)}-${seed}")
    t.mkdirs
    t
  }
}
