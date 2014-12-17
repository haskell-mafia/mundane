package com.ambiata.mundane
package io

import com.ambiata.mundane.control._
import java.io._
import scalaz.effect.IO
import MemoryConversions._

object Streams {
  val DefaultChunkSize = 1.mb.toBytes.value.toInt

  def read(in: InputStream, encoding: String = "UTF-8"): RIO[String] =
    readBytes(in).map(new String(_, encoding))

  def write(out: OutputStream, data: String, encoding: String = "UTF-8"): RIO[Unit] =
    RIO.safe(writeToStream(out, data, encoding))

  def writeToStream(out: OutputStream, data: String, encoding: String = "UTF-8") = {
    val writer = new PrintStream(out, false, encoding)
    try     writer.print(data)
    finally writer.close
  }

  def readBytes(in: InputStream, chunksize: Int = DefaultChunkSize): RIO[Array[Byte]] =
    RIO.safe(readFromStream(in, chunksize))

  def readFromStream(in: InputStream, chunksize: Int = DefaultChunkSize): Array[Byte] = {
    val buffer = Array.ofDim[Byte](chunksize)
    val out = new ByteArrayOutputStream
    var chunk = 0
    while ({ chunk = in.read(buffer); chunk != -1 })
      out.write(buffer, 0, chunk)
    out.toByteArray
  }

  def writeBytes(out: OutputStream, data: Array[Byte]): RIO[Unit] = RIO.safe {
    out.write(data, 0, data.length)
  }

  def pipe(in: InputStream, out: OutputStream, chunksize: Int = DefaultChunkSize): RIO[Unit] = RIO.safe {
    val buffer = Array.ofDim[Byte](chunksize)
    var chunk = 0
    while ({ chunk = in.read(buffer); chunk != -1 })
      out.write(buffer, 0, chunk)
  }
}
