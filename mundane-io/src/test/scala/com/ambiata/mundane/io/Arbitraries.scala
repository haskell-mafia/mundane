package com.ambiata.mundane.io

import org.scalacheck._
import scala.io.Codec
import scalaz._, effect.IO

object Arbitraries {
  implicit def LocalTemporaryArbitrary: Arbitrary[LocalTemporary] = Arbitrary(for {
    i <- Gen.choose(1, 5)
    a <- Gen.listOfN(i, Gen.identifier)
    z = a.mkString("/")
    f <- Gen.oneOf("", "/")
  } yield LocalTemporary(Temporary.uniqueLocalPath, s"temporary-${java.util.UUID.randomUUID().toString}/" + z + f))

  implicit def CodecArbitrary: Arbitrary[Codec] = Arbitrary(Gen.oneOf(
      Codec.UTF8
    , Codec.ISO8859
  ))
}
