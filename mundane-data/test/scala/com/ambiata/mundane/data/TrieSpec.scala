package com.ambiata.mundane.data

import org.specs2.mutable.Specification

class TrieSpec extends Specification {

  "A trie can be used to store values using string prefixes" >> {
    val trie = Trie[Int]()
    trie.put("a", 1)
    trie.put("abc", 2)
    trie.put("abd", 3)

    trie.get("a")   === Some(1)
    trie.get("ab")  === None
    trie.get("abc") === Some(2)
    trie.get("abd") === Some(3)
  }

  "It is possible to take the size of a large trie" >> {
    val trie = Trie[Int]()
    val size = 20000
    (1 to size).foreach(i => trie.put(i.toString, i))
    trie.size === size
  }

}
