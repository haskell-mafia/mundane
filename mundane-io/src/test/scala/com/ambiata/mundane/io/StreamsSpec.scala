package com.ambiata.mundane
package io

import java.io._
import org.specs2._

class StreamsSpec extends Specification with ScalaCheck { def is = s2"""

Streams should be able to:
  read string from stream                       $read
  write string to stream                        $write
  read & write strings symmetrically            $roundtrip
  read bytes from stream                        $bytes

"""

  def read = prop((s: String) => {
    val in = new ByteArrayInputStream(s.getBytes("UTF-8"))
    Streams.read(in, "UTF-8") === s
  })

  def write = prop((s: String) => {
    val out = new ByteArrayOutputStream()
    Streams.write(out, s)
    out.toByteArray === s.getBytes("UTF-8")
  })

  def roundtrip = prop((s: String) => {
    val out = new ByteArrayOutputStream()
    Streams.write(out, s, "UTF-8")
    val in = new ByteArrayInputStream(out.toByteArray)
    Streams.read(in, "UTF-8") === s
  })

  def bytes =  prop((bs: Array[Byte]) => {
    val in = new ByteArrayInputStream(bs)
    Streams.bytes(in) === bs
  })
}
