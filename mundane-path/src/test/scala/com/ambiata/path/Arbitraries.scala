package com.ambiata.mundane.path

import org.scalacheck._
import Arbitrary._
import scalaz._, Scalaz._, effect.IO
import com.ambiata.mundane.control._
import com.ambiata.mundane.io._

object Arbitraries {
  implicit def LocalTemporaryArbitrary: Arbitrary[LocalTemporary] = Arbitrary(for {
    i <- Gen.choose(1, 5)
    a <- Gen.listOfN(i, Gen.identifier)
    z = a.mkString("/")
    f <- Gen.oneOf("", "/")
  } yield LocalTemporary(s"temporary-${java.util.UUID.randomUUID().toString}/" + z + f))

  implicit def FileNameArbitrary: Arbitrary[FileName] = Arbitrary(
    Gen.identifier map FileName.unsafe
  )

  implicit def PathArbitrary: Arbitrary[Path] = Arbitrary(Gen.frequency(
    1 -> Gen.const(Root)
  , 1 -> Gen.const(Relative)
  , 4  -> (for { n <- arbitrary[FileName];  b <- arbitrary[Path] } yield Component(b, n))
  ))
}
