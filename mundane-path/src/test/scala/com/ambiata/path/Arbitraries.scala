package com.ambiata.mundane.path

import org.scalacheck._
import Arbitrary._
import scalaz._, Scalaz._, effect.IO
import com.ambiata.mundane.control._

object Arbitraries {

  implicit def ComponentArbitrary: Arbitrary[Component] = Arbitrary(
    Gen.identifier map Component.unsafe)

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
    Gen.listOf(arbitrary[Component]).map(Path.fromList(base, _))
}
