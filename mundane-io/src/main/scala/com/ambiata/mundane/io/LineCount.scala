package com.ambiata.mundane
package io

import java.io._
import scalaz._, Scalaz._
import scalaz.effect._

case class LineCount(file: File, count: Int)

object LineCount {
  def count(file: File): IO[LineCount] = IO {
    val reader = new LineNumberReader(new FileReader(file))
    try {
      while (reader.readLine != null) {}
      LineCount(file, reader.getLineNumber)
    } finally reader.close
  }
}
