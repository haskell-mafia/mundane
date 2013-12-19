package com.ambiata.mundane.data

import scala.collection.immutable.TreeMap

/**
 * This is a mutable Trie where it is possible to pass a normalisation function to deal with similar keys
 */
case class Trie[V](key: Option[Char] = None, normalise: String => String = (s: String) => s) {

  private var nodes: TreeMap[Char, Trie[V]] = new TreeMap[Char, Trie[V]]
  private var value : Option[V] = None

  def put(k: String, v: V): Trie[V] = {
    val normalised = normalise(k)
    if (normalised.isEmpty) { value = Some(v); this }
    else {
      val first = normalised.charAt(0)
      nodes.get(first).getOrElse {
        val n = Trie[V](Some(first), normalise)
        nodes = nodes.insert(first, n)
        n
      }.put(normalised.substring(1), v)
    }
  }

  def contains(k: String): Boolean = get(k).isDefined

  def get(k: String): Option[V] = nodeFor(k).flatMap(_.value)
  def getAll : Seq[V] = this.value.toList ++ this.nodes.values.flatMap(_.getAll)
  def getAllWithPrefix(k: String) : Seq[V] = nodeFor(k).toList.flatMap(_.getAll)

  override def toString =
    key.toString+":"+value+"\n"+nodes.values.map(_.toString.split("\n").map("  "+_).mkString("\n")).mkString("\n")

  def nodeFor(k: String): Option[Trie[V]] =  {
    val normalised = normalise(k)
    if (normalised.isEmpty) Some(this)
    else                    nodes.get(normalised.charAt(0)).flatMap(_.nodeFor(normalised.substring(1)))
  }
}
