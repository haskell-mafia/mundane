package com.ambiata.mundane
package io

import java.io._

object Streams {
  val DefaultChunkSize = 4096

  def read(in: InputStream, encoding: String = "UTF-8") =
    new String(bytes(in), encoding)

  def write(out: OutputStream, data: String, encoding: String = "UTF-8") = {
    val writer = new PrintStream(out, false, encoding);
    try writer.print(data)
    finally writer.close
  }

  def bytes(in: InputStream, chunksize: Int = DefaultChunkSize): Array[Byte] = {
    val buffer = Array.ofDim[Byte](chunksize)
    val out = new ByteArrayOutputStream
    var chunk = 0
    while ({ chunk = in.read(buffer); chunk != -1 })
      out.write(buffer, 0, chunk)
    out.toByteArray
  }
  
  def pipeToFile(in: InputStream, filename: File, chunksize: Int = DefaultChunkSize): Unit = {
    val buffer = Array.ofDim[Byte](chunksize)
    val out = new FileOutputStream(filename)
    var chunk = 0
    while ({ chunk = in.read(buffer); chunk != -1 })
      out.write(buffer, 0, chunk)
  }
}
