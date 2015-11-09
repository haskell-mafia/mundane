package com.ambiata.mundane.testing

import org.scalacheck._

import scala.collection.JavaConversions._

case class KeyEntry(path: String, value: Int) {
  def prepend(string: String) = copy(string+"/"+path)
  def full =
    path + "/" + value.toString
}

case class Keys(keys: List[KeyEntry]) {
  def map(f: KeyEntry => KeyEntry) = copy(keys.map(f))
}

object Keys {
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

  def key: Gen[String] = for {
    n     <- Gen.choose(1, 3)
    parts <- Gen.listOfN(n, component)
  } yield parts.mkString("/")

  def entry(prefix: String): Gen[KeyEntry] = for {
    p <- key
    n <- Gen.choose(1, 1000000)
  } yield KeyEntry(s"$prefix/$p", n)

  def entries(prefix: String, n: Int = 10): Gen[List[KeyEntry]] =
    if (n <= 0) Gen.const(Nil) else
      Gen.frequency(
        0 -> (for {
          n1       <- Gen.choose(1, n)
          children <- Gen.listOfN(n1, key)
          r        <- Gen.sequence(children.map(c => entries(c, n1 / 2)))
        } yield r.toList.flatten)
        , 1 -> (for {
          n1        <- Gen.choose(1, n)
          children <- Gen.listOfN(n1, entry(prefix))
        } yield children)
      )

  implicit def PathsArbitrary: Arbitrary[Keys] =
    Arbitrary(for { prefix <- key; es <- entries(prefix) } yield Keys(es))
}

object KeyEntry {
  import Keys._

  implicit def EntryArbitrary: Arbitrary[KeyEntry] =
    Arbitrary(for { prefix <- key; e <- entry(prefix) } yield e)
}
