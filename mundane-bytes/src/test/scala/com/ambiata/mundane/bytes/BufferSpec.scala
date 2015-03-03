package com.ambiata.mundane.bytes

import com.ambiata.disorder._
import org.scalacheck.{Arbitrary, Gen}
import org.specs2.{ScalaCheck, Specification}

class BufferSpec extends Specification with ScalaCheck { def is = s2"""

  Creating an empty buffer is the same as wrapping that array with no offset or length
    $wrapArrayAndEmpty

  Allocating less then the array length will return the same instance
    $allocateNoResize

  Allocating more then the array length will instantiate a new instance
    $allocateResize

  Shifting the offset succeeds when the value is less than the buffer length
    $shiftSuccess

  Shifting the offset fails when the value is greater than the buffer length
    $shiftFail

  Allocated buffer will increment the length
    $allocateLength

  Allocated buffer will always have at least the required capacity
    $allocateHasCapacity

  Allocated buffer will always have the same bytes up to the original buffer length
    $allocateCopy

  Slice returns an exact fit of the buffer data
    $slice

  Copy the buffer data into another array
    $copy
"""

  def wrapArrayAndEmpty = prop((n: NaturalIntSmall) => {
    val b = Buffer.empty(n.value)
    b ==== Buffer.wrapArray(b.bytes, 0, 0)
  })

  def allocateNoResize = prop((n: OrderedPair[NaturalIntSmall]) => {
    val b1 = Buffer.empty(n.second.value)
    val b2 = Buffer.allocate(b1, n.first.value)
    b1 must beTheSameAs(b2) and (b1.bytes must beTheSameAs(b2.bytes))
  })

  def allocateResize = prop((n: OrderedPair[NaturalIntSmall]) => n.first != n.second ==> {
    val b1 = Buffer.empty(n.first.value)
    val b2 = Buffer.allocate(b1, n.second.value)
    b2 must not beTheSameAs(b1) and (b2.bytes must not beTheSameAs(b1.bytes))
  })

  def shiftSuccess = prop((b: Buffer, n: NaturalIntSmall) => b.length > 0 ==> {
    val o = n.value % b.length
    Buffer.shift(b, o).offset ==== (b.offset + o)
  })

  def shiftFail = prop((b: Buffer, n: NaturalIntSmall) =>
    Buffer.shift(b, b.length + n.value) must throwA[RuntimeException]
  )

  def allocateLength = prop((b1: Buffer, n: NaturalIntSmall) => {
    val length = b1.length
    val b2 = Buffer.allocate(b1, n.value)
    b2.length ==== length + n.value
  })

  def allocateHasCapacity = prop((b1: Buffer, n: NaturalIntSmall) => {
    val length = b1.length
    val b2 = Buffer.allocate(b1, n.value)
    b2.bytes.length must beGreaterThanOrEqualTo(length + n.value)
  })

  def allocateCopy = prop((b1: Buffer, n: NaturalIntSmall) => b1.bytes.length > 0 ==> {
    val b2 = Buffer.allocate(b1, n.value)
    b2.bytes.toList.take(b1.bytes.length) ==== b1.bytes.toList
  })

  def slice = prop((b: Buffer) =>
    Buffer.slice(b).toList ==== b.bytes.toList.slice(b.offset, b.offset + b.length)
  )

  def copy = prop((b: Buffer, a: Array[Byte], n: NaturalIntSmall) => (b.length + n.value) <= a.length  ==> {
    Buffer.copy(b, a, n.value)
    a.toList.slice(n.value, n.value + b.length) ==== Buffer.slice(b).toList
  })

  implicit def BufferArbitrary: Arbitrary[Buffer] =
    Arbitrary(for {
      b <- Arbitrary.arbitrary[Array[Byte]]
      o <- Gen.choose(0, b.length - 1)
      l <- Gen.choose(0, b.length - o)
    } yield Buffer.wrapArray(b, o, l))
}
