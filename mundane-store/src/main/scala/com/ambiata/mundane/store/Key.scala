package com.ambiata.mundane.store

/**
 * Key to access a value in a key-value Store
 */
case class Key(components: Vector[KeyName]) {
  
  def /(keyName: KeyName): Key =
    copy(components = components :+ keyName)

  def /(key: Key): Key =
    copy(components = components ++ key.components)

  def fromRoot: Key =
    copy(components = components.tail)

  def name: String =
    if (components.isEmpty) "/"
    else                    components.map(_.name).mkString("/")
}

object Key {

  def apply(name: KeyName): Key =
    Root / name

  val Root = Key(Vector())

  def unsafe(s: String): Key =
    new Key(s.split("/").toVector.map(KeyName.unsafe))

}