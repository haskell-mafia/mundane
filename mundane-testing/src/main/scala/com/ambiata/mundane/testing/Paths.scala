package com.ambiata.mundane.testing

import com.ambiata.mundane.control._
import java.io._
import org.scalacheck._, Arbitrary._
import scalaz._, Scalaz._
import scalaz.effect.IO

case class Entry(path: String, value: Int)
case class Paths(entries: List[Entry])

object Paths {
  /* food adjectives */
  def food: Gen[String] =
    Gen.oneOf("activated", "sweet", "savoury", "unsweetended", "spicy", "bland", "skinned")

  /* muppets */
  def muppets: Gen[String] =
    Gen.oneOf("kermit", "statler", "waldorf", "gonzo", "scooter", "fozzie", "animal")

  def component: Gen[String] = for {
    f <- food
    m <- muppets
  } yield s"$f-$m"

  def path: Gen[String] = for {
    n     <- Gen.choose(1, 3)
    parts <- Gen.listOfN(n, component)
  } yield parts.mkString("/")

  def entry(prefix: String): Gen[Entry] = for {
    p <- path
    n <- Gen.choose(1, 1000000)
  } yield Entry(s"$prefix/$p", n)

  def entries(prefix: String): Gen[List[Entry]] =
    Gen.frequency(
      2 -> (for {
        n        <- Gen.choose(1, 10)
        children <- Gen.listOfN(n, path)
        r        <- children.foldRight(Gen.oneOf(List[List[List[Entry]]]()))((el, acc) => for { accx <- acc; x <- entries(el) } yield x :: accx)
      } yield r.flatten)
    , 1 -> (for {
        n        <- Gen.choose(1, 10)
        children <- Gen.listOfN(n, entry(prefix))
      } yield children)
    )

  implicit def EntryArbitrary: Arbitrary[Entry] =
    Arbitrary(for { prefix <- path; e <- entriy(prefix) } yield e)

  implicit def PathsArbitrary: Arbitrary[Paths] =
    Arbitrary(for { prefix <- path; es <- entries(prefix) } yield Paths(es))
}
