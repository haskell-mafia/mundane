package com.ambiata.mundane.store

import scalaz.Tree

/**
 * Key to access a value in a key-value Store
 */
case class Key(components: Vector[KeyName]) {
  
  def </>(keyName: KeyName): Key =
    copy(components = components :+ keyName)

  def </>(key: Key): Key =
    copy(components = components ++ key.components)

  def fromRoot: Key =
    copy(components = components.tail)
}

case class KeyName(name: String) extends AnyVal

object Key {

  def fromKeyName(name: KeyName): Key =
    Root </> name

  def fromName(name: String): Key =
    Root </> KeyName(name)

  val Root = Key(Vector())
  
  def asTree(keys: List[Key]): Tree[Key] = ???
}



