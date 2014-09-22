package com.ambiata.mundane.store

import org.specs2.Specification
import org.specs2.matcher.Matcher
import Key._

class KeySpec extends Specification { def is = s2"""

 A key is a list of key names
   its name is the concatenation of all the keynames $name

 Keys can be appended                $appended
 A Key can be appended to a key name $appendedToKeyName

"""

  def name = {
    val key: Key = "a" / "b" / "c"
    key must haveName("a/b/c")
  }

  def appended = {
    val key1: Key = "a" / "b"
    val key2: Key = "c" / "d"
    (key1 / key2) must haveName("a/b/c/d")
  }

  def appendedToKeyName = {
    val key1: Key = "b" / "c"
    ("a" / key1) must haveName("a/b/c")
  }

  def haveName(name: String): Matcher[Key] = { key: Key =>
    (key.name == name, s"$key doesn't have name $name")
  }
}
