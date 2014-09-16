package com.ambiata.mundane.testing

import com.ambiata.mundane.control._
import java.io._
import org.scalacheck._, Arbitrary._
import scalaz._, Scalaz._
import scalaz.effect.IO
import scala.collection.JavaConversions._

case class Entry(path: String, value: Int) {
  def prepend(string: String) = copy(string+"/"+path)
  def full =
    "/" + path + "/" + value.toString
}
case class Paths(entries: List[Entry]) {
  def map(f: Entry => Entry) = copy(entries.map(f))
}

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

  def entries(prefix: String, n: Int = 10): Gen[List[Entry]] =
    if (n <= 0) Gen.const(Nil) else
      Gen.frequency(
        0 -> (for {
          n1       <- Gen.choose(1, n)
          children <- Gen.listOfN(n1, path)
          r        <- Gen.sequence(children.map(c => entries(c, n1 / 2)))
        } yield r.toList.flatten)
        , 1 -> (for {
          n1        <- Gen.choose(1, n)
          children <- Gen.listOfN(n1, entry(prefix))
        } yield children)
      )

  implicit def PathsArbitrary: Arbitrary[Paths] =
    Arbitrary(for { prefix <- path; es <- entries(prefix) } yield Paths(es))
}

object Entry {
  import Paths._

  implicit def EntryArbitrary: Arbitrary[Entry] =
    Arbitrary(for { prefix <- path; e <- entry(prefix) } yield e)
}
