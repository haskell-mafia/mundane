package com.ambiata.mundane
package io

import com.ambiata.mundane.control._
import java.io._
import scalaz.effect.IO

object Streams {
  val DefaultChunkSize = 4096

  def read(in: InputStream, encoding: String = "UTF-8"): ResultT[IO, String] =
    readBytes(in).map(new String(_, encoding))

  def write(out: OutputStream, data: String, encoding: String = "UTF-8"): ResultT[IO, Unit] =
    ResultT.safe(writeToStream(out, data, encoding))

  def writeToStream(out: OutputStream, data: String, encoding: String = "UTF-8") = {
    val writer = new PrintStream(out, false, encoding)
    try     writer.print(data)
    finally writer.close
  }

  def readBytes(in: InputStream, chunksize: Int = DefaultChunkSize): ResultT[IO, Array[Byte]] =
    ResultT.safe(readFromStream(in, chunksize))

  def readFromStream(in: InputStream, chunksize: Int = DefaultChunkSize): Array[Byte] = {
    val buffer = Array.ofDim[Byte](chunksize)
    val out = new ByteArrayOutputStream
    var chunk = 0
    while ({ chunk = in.read(buffer); chunk != -1 })
      out.write(buffer, 0, chunk)
    out.toByteArray
  }

  def writeBytes(out: OutputStream, data: Array[Byte]): ResultT[IO, Unit] = ResultT.safe {
    out.write(data, 0, data.length)
  }

  def pipe(in: InputStream, out: OutputStream, chunksize: Int = DefaultChunkSize): ResultT[IO, Unit] = ResultT.safe {
    val buffer = Array.ofDim[Byte](chunksize)
    var chunk = 0
    while ({ chunk = in.read(buffer); chunk != -1 })
      out.write(buffer, 0, chunk)
  }
}
