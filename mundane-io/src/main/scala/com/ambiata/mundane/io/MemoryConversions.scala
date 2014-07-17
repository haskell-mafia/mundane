package com.ambiata
package mundane
package io

import MemoryConversions._
/**
 * Conversion units for memory quantities
 */
trait MemoryConversions {
  implicit class bytesSyntax(value: Long) {
    def bytes = Bytes(value)
    def byte  = Bytes(value)
    def kb    = Kilobytes(value)
    def kbs   = Kilobytes(value)
    def mb    = Megabytes(value)
    def mbs   = Megabytes(value)
    def gb    = Gigabytes(value)
    def gbs   = Gigabytes(value)
    def tb    = Terabytes(value)
    def tbs   = Terabytes(value)
  }
}

sealed trait BytesQuantity {
  def value: Long
  def toBytes: Bytes
  def toKilobytes: Kilobytes
  def toMegabytes: Megabytes
  def toGigabytes: Gigabytes
  def toTerabytes: Terabytes
  def show =
       if (toTerabytes.value > 0) toTerabytes.value+"Tb"
  else if (toGigabytes.value > 0) toGigabytes.value+"Gb"
  else if (toMegabytes.value > 0) toMegabytes.value+"Mb"
  else if (toKilobytes.value > 0) toKilobytes.value+"Mb"
  else                            toBytes.value+" bytes"
}

object BytesQuantity {
  implicit def Numeric: Numeric[BytesQuantity] = new Numeric[BytesQuantity] {
    def plus(x: BytesQuantity, y: BytesQuantity): BytesQuantity = (x.toBytes.value + y.toBytes.value).bytes
    def toDouble(x: BytesQuantity): Double = x.toBytes.value.toDouble
    def toFloat(x: BytesQuantity): Float = x.toBytes.value.toFloat
    def toInt(x: BytesQuantity): Int = x.toBytes.value.toInt
    def negate(x: BytesQuantity): BytesQuantity = (- x.toBytes.value).bytes
    def fromInt(x: Int): BytesQuantity = Bytes(x.toLong)
    def toLong(x: BytesQuantity): Long = x.toBytes.value
    def times(x: BytesQuantity, y: BytesQuantity): BytesQuantity = (x.toBytes.value * y.toBytes.value).bytes
    def minus(x: BytesQuantity, y: BytesQuantity): BytesQuantity = (x.toBytes.value - y.toBytes.value).bytes
    def compare(x: BytesQuantity, y: BytesQuantity): Int = x.toBytes.value.compare(y.toBytes.value)
  }
}

case class Bytes(value: Long) extends BytesQuantity {
  def toBytes    = this
  def toKilobytes = Kilobytes(value / 1024)
  def toMegabytes = toKilobytes.toMegabytes
  def toGigabytes = toKilobytes.toGigabytes
  def toTerabytes = toKilobytes.toTerabytes
}
case class Kilobytes(value: Long) extends BytesQuantity {
  def toBytes     = Bytes(value * 1024)
  def toKilobytes = this
  def toMegabytes = Megabytes(value / 1024)
  def toGigabytes = toMegabytes.toGigabytes
  def toTerabytes = toMegabytes.toTerabytes
}
case class Megabytes(value: Long) extends BytesQuantity {
  def toBytes     = toKilobytes.toBytes
  def toKilobytes = Kilobytes(value * 1024)
  def toMegabytes = this
  def toGigabytes = Gigabytes(value / 1024)
  def toTerabytes = toGigabytes.toTerabytes
}
case class Gigabytes(value: Long) extends BytesQuantity {
  def toBytes     = toMegabytes.toBytes
  def toKilobytes = toMegabytes.toKilobytes
  def toMegabytes = Megabytes(value * 1024)
  def toGigabytes = this
  def toTerabytes = Terabytes(value / 1024)
}
case class Terabytes(value: Long) extends BytesQuantity {
  def toBytes     = toGigabytes.toBytes
  def toKilobytes = toGigabytes.toKilobytes
  def toMegabytes = toGigabytes.toMegabytes
  def toGigabytes = Gigabytes(value * 1024)
  def toTerabytes = this
}

object MemoryConversions extends MemoryConversions
