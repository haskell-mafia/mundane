package com.ambiata.mundane
package io

import com.ambiata.disorder._
import com.ambiata.mundane.io.Arbitraries._
import com.ambiata.mundane.testing.RIOMatcher._
import java.io._
import org.specs2._
import scala.io.Codec
import scalaz._, Scalaz._

class StreamsSpec extends Specification with ScalaCheck { def is = s2"""

Streams should be able to:
  read string from stream                       $read
  read string from stream with codec            $readCodec
  read bytes from stream                        $readBytes
  write string to stream                        $write
  write string to stream with codec             $writeCodec
  write bytes to stream                         $writeBytes
  read & write strings symmetrically            $roundtrip
  pipe from stream to stream                    $pipe

"""

  def read = prop((s: S) => {
    val in = new ByteArrayInputStream(s.value.getBytes(Codec.UTF8.name))
    Streams.read(in) must beOkValue(s.value)
  })

  def readCodec = prop((s: S, c: Codec) => validForCodec(s, c) ==> {
    val in = new ByteArrayInputStream(s.value.getBytes(c.name))
    Streams.readWithEncoding(in, c) must beOkValue(s.value)
  })

  def readBytes = prop((bs: Array[Byte]) => {
    val in = new ByteArrayInputStream(bs)
    Streams.readBytes(in) must beOkValue(bs)
  })

  def write = prop((s: S) => {
    val out = new ByteArrayOutputStream()
    Streams.write(out, s.value).map(_ => out.toByteArray) must beOkValue(s.value.getBytes(Codec.UTF8.name))
  })

  def writeCodec = prop((s: S, c: Codec) => validForCodec(s, c) ==> {
    val out = new ByteArrayOutputStream()
    Streams.writeWithEncoding(out, s.value, c).map(_ => out.toByteArray) must beOkValue(s.value.getBytes(c.name))
  })

  def writeBytes = prop((bs: Array[Byte]) => {
    val out = new ByteArrayOutputStream()
    def in = new ByteArrayInputStream(out.toByteArray)
    val action = Streams.writeBytes(out, bs) >> Streams.readBytes(in)
  })

  def roundtrip = prop((s: String) => {
    val out = new ByteArrayOutputStream()
    def in = new ByteArrayInputStream(out.toByteArray)
    val action = Streams.write(out, s) >> Streams.read(in)
    action must beOkValue(s)
  })

  def pipe = prop((s: Array[Byte]) => {
    val out = new ByteArrayOutputStream()
    val in = new ByteArrayInputStream(s)
    val action = Streams.pipe(in, out).as(out.toByteArray)
    action must beOkValue(s)
  })

  def validForCodec(s: S, c: Codec): Boolean =
    new String(s.value.getBytes(c.name), c.name) == s.value

}
