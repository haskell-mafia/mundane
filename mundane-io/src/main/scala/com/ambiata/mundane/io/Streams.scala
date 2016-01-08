package com.ambiata.mundane
package io

import com.ambiata.mundane.control._
import java.io._
import scala.io.Codec
import scalaz.effect.IO
import MemoryConversions._

object Streams {
  val DefaultChunkSize = 1.mb.toBytes.value.toInt

  def read(in: InputStream): RIO[String] =
    readWithEncoding(in, Codec.UTF8)

  def readWithEncoding(in: InputStream, encoding: Codec): RIO[String] =
    readBytes(in).map(new String(_, encoding.name))

  def write(out: OutputStream, data: String): RIO[Unit] =
    writeWithEncoding(out, data, Codec.UTF8)

  def writeWithEncoding(out: OutputStream, data: String, encoding: Codec): RIO[Unit] =
    RIO.safe(writeToStreamWithEncoding(out, data, encoding))

  def writeToStreamWithEncoding(out: OutputStream, data: String, encoding: Codec) = {
    val writer = new PrintStream(out, false, encoding.name)
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
