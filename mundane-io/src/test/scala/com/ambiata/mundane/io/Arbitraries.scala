package com.ambiata.mundane.io

import org.scalacheck._, Arbitrary._

object Arbitraries {
  implicit def FileNameArbitrary: Arbitrary[FileName] = Arbitrary(
    Gen.identifier map FileName.unsafe
  )

  implicit def PathArbitrary: Arbitrary[Path] = Arbitrary(for {
    base <- Gen.oneOf(Root, Relative)
    path <- genPathFrom(base)
  } yield path)

  case class RelativePath(path: Path)

  implicit def RelativePathArbitrary: Arbitrary[RelativePath] =
    Arbitrary(genPathFrom(Relative) map RelativePath)

  case class AbsolutePath(path: Path)

  implicit def AbsolutePathArbitrary: Arbitrary[AbsolutePath] =
    Arbitrary(genPathFrom(Root) map AbsolutePath)

  def genPathFrom(base: Path): Gen[Path] =
    Gen.listOf(arbitrary[FileName]).map(Path.fromList(base, _))
}
