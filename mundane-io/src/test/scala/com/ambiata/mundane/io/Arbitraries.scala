package com.ambiata.mundane.io

import org.scalacheck._, Arbitrary._

object Arbitraries {
  implicit def FileNameArbitrary: Arbitrary[FileName] = Arbitrary(
    Gen.identifier map FileName.unsafe
  )

  implicit def PathArbitrary: Arbitrary[Path] = Arbitrary(Gen.frequency(
    1 -> Gen.const(Root)
  , 1 -> Gen.const(Relative)
  , 4  -> (for { n <- arbitrary[FileName];  b <- arbitrary[Path] } yield Component(b, n))
  ))
}
