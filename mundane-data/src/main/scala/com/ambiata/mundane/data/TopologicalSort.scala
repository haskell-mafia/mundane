package com.ambiata.mundane.data

import org.kiama.attribution.AttributionCore

/**
 * This trait sorts "Nodes" given a "Successor" relationship and a "Same" relationship
 *
 * it places in different sequences nodes which might have dependencies with each other so that
 * it is guaranteed that:
 *
 *    nodes in the same layer <=> there's no successor relationship between the nodes
 *    if the nodes don't have circular dependencies then the layers don't have circular dependencies either
 */
trait TopologicalSort extends AttributionCore {

  type Node <: AnyRef
  implicit val successorRelationship: Successor[Node]
  implicit val isSameRelationship: IsSame[Node]

  import successorRelationship._
  import isSameRelationship._

  def sort(nodes: Seq[Node]): Seq[Seq[Node]] = {
    val (leaves, nonLeaves) = all(nodes).partition((n: Node) => descendents(n).isEmpty)
    val result = leaves +:
      nonLeaves.groupBy(longestPathSizeTo(leaves)).toSeq.sortBy(_._1).map(_._2)
    result.filterNot(_.isEmpty).distinct
  }

  def all(nodes: Seq[Node]) = (nodes ++ nodes.flatMap(descendents)).distinct

  lazy val children: CachedAttribute[Node, Seq[Node]] = attr(previous)

  lazy val descendents : CachedAttribute[Node, Seq[Node]] = attr { case node =>
    (children(node) ++ children(node).flatMap(descendents)).distinct
  }

  lazy val longestPathSizeTo: Seq[Node] => Node => Int = paramAttr { (target: Seq[Node]) => node: Node =>
    if (target.isEmpty) -1 else target.map(t => longestPathSizeToNode(t)(node)).max
  }

  lazy val longestPathSizeToNode: Node => Node => Int = paramAttr { target: Node => node: Node =>
    longestPathToNode(target)(node).size
  }

  lazy val longestPathToNode: Node => Node => Seq[Node] = paramAttr { target: Node => node: Node =>
    if (isSame(node, target))        Seq(node)  // found
    else if (children(node).isEmpty) Seq()      // not found
    else                             node +: children(node).map(longestPathToNode(target)).maxBy(_.size)
  }
}


trait Successor[T] {
  def previous(t: T): Seq[T]
}
trait IsSame[T] {
  def isSame(t1: T, t2: T): Boolean
}
