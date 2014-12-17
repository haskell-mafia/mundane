package com.ambiata.mundane.path

import org.scalacheck._
import Arbitrary._
import scalaz._, Scalaz._, effect.IO
import com.ambiata.mundane.control._
import com.ambiata.mundane.io._

object Arbitraries {
<<<<<<< HEAD
  implicit def LocalTemporaryArbitrary: Arbitrary[LocalTemporary] = Arbitrary(for {
    i <- Gen.choose(1, 5)
    a <- Gen.listOfN(i, Gen.identifier)
    z = a.mkString("/")
    f <- Gen.oneOf("", "/")
  } yield LocalTemporary(s"temporary-${java.util.UUID.randomUUID().toString}/" + z + f))

  implicit def FileNameArbitrary: Arbitrary[FileName] = Arbitrary(
    Gen.identifier map FileName.unsafe
=======
  implicit def ComponentArbitrary: Arbitrary[Component] = Arbitrary(
    Gen.identifier map Component.unsafe
>>>>>>> 0e636c0... Local data types
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
    Gen.listOf(arbitrary[Component]).map(Path.fromList(base, _))
}
