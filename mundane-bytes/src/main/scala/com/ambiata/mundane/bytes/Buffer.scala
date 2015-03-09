package com.ambiata.mundane.bytes

/**
 * <pre>
 *                  uuuuuuu
 *               uu$$$$$$$$$$$uu
 *           uu$$$$$$$$$$$$$$$$$uu
 *          u$$$$$$$$$$$$$$$$$$$$$u
 *         u$$$$$$$$$$$$$$$$$$$$$$$u
 *        u$$$$$$$$$$$$$$$$$$$$$$$$$u
 *        u$$$$$$$$$$$$$$$$$$$$$$$$$u
 *        u$$$$$$"   "$$$"   "$$$$$$u
 *        "$$$$"      u$u       $$$$"
 *         $$$u       u$u       u$$$
 *         $$$u      u$$$u      u$$$
 *          "$$$$uu$$$   $$$uu$$$$"
 *           "$$$$$$$"   "$$$$$$$"
 *             u$$$$$$$u$$$$$$$u
 *              u$"$"$"$"$"$"$u
 *   uuu        $$u$ $ $ $ $u$$       uuu
 *  u$$$$        $$$$$u$u$u$$$       u$$$$
 *   $$$$$uu      "$$$$$$$$$"     uu$$$$$$
 * u$$$$$$$$$$$uu    """""    uuuu$$$$$$$$$$
 * $$$$"""$$$$$$$$$$uuu   uu$$$$$$$$$"""$$$"
 *  """      ""$$$$$$$$$$$uu ""$"""
 *            uuuu ""$$$$$$$$$$uuu
 *   u$$$uuu$$$$$$$$$uu ""$$$$$$$$$$$uuu$$$
 *   $$$$$$$$$$""""           ""$$$$$$$$$$$"
 *    "$$$$$"                      ""$$$$""
 *      $$$"                         $$$$"
 * </pre>
 *
 * This data type is _not_ intended as a safe, immutable array of bytes like
 * [[https://github.com/scodec/scodec-bits/blob/master/core/src/main/scala/scodec/bits/ByteVector.scala ByteVector]].
 * Instead, this is a c-like struct for wrapping an array with an offset/length.
 * In other words - it's bringing convenience to crazy-town, not correctness.
 *
 * The important constraint is that both [[bytes]] and [[offset]] are immutable, and a new copy of the [[Buffer]] needs
 * to created to resize or offset the array. Consumers can then do an instance check on [[Buffer]] to confirm
 * whether the wrapped bytes (may) have been modified or whether an extra/final copy needs to be done.
 */
case class Buffer private(bytes: Array[Byte], offset: Int, private var _length: Int) {

  def length: Int =
    _length
}

object Buffer {

  def empty(initialSize: Int): Buffer =
    wrapArray(new Array[Byte](initialSize), 0, 0)

  def wrapArray(bytes: Array[Byte], offset: Int, length: Int): Buffer = {
    if (offset < 0 || offset >= bytes.length)
      sys.error(s"Invalid offset $offset for array of length ${bytes.length}")
    if (length < 0 || offset + length > bytes.length)
      sys.error(s"Invalid length $length with offset $offset and array of length ${bytes.length}")
    new Buffer(bytes, offset, length)
  }

  /** Shifts the offset, effectively shrinking the buffer and preserving the current length position */
  def shift(b: Buffer, o: Int): Buffer = {
    if (o < 0)
      sys.error(s"Offset can only be increased: $o")
    wrapArray(b.bytes, b.offset + o, b.length - o)
  }

  /** Ensure there is enough space in the array, and if not return a _new_ [[Buffer]] and array with the original values */
  def allocate(b1: Buffer, i: Int): Buffer =
    if (b1.length + i <= b1.bytes.length) {
      b1._length += i
      b1
    }
    else {
      val b2 = new Buffer(new Array(math.max(b1.bytes.length + i, b1.bytes.length * 2)), b1.offset, b1.length + i)
      System.arraycopy(b1.bytes, 0, b2.bytes, 0, b1.bytes.length)
      b2
    }

  /** Returns an exact-fitting array based on the offset/length */
  def slice(b: Buffer): Array[Byte] = {
    val a = new Array[Byte](b.length)
    copy(b, a, 0)
    a
  }

  def copy(source: Buffer, target: Array[Byte], offset: Int): Unit =
    System.arraycopy(source.bytes, source.offset, target, offset, source.length)
}
