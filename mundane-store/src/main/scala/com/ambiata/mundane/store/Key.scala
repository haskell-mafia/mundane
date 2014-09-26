package com.ambiata.mundane.store

/**
 * Key to access a value in a key-value Store
 */
case class Key(components: Vector[KeyName]) {

  def prepend(keyName: KeyName): Key =
    copy(components = keyName +: components)

  def /(keyName: KeyName): Key =
    copy(components = components :+ keyName)

  def /(key: Key): Key =
    copy(components = components ++ key.components)

  def fromRoot: Key =
    copy(components = components.tail)

  def head: Key =
    copy(components = components.take(1))

  def name: String =
    components.map(_.name).mkString("/")
}

object Key {

  def apply(name: KeyName): Key =
    Root / name

  val Root = Key(Vector())

  def unsafe(s: String): Key =
    new Key(s.split("/").toVector.map(KeyName.unsafe))

  /** for now this can't return None because the only value we exclude from a KeyName is / */
  def fromString(s: String): Option[Key] =
    Some(new Key(s.split("/").toVector.map(KeyName.unsafe)))

}