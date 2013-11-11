package com.ambiata
package mundane
package data

import org.specs2._
import scalaz._, Scalaz._
import org.scalacheck._
import matcher._

class TopologicalSortSpec extends Specification with ScalaCheck with TopologicalSort { def is = sequential ^ s2"""

  2 nodes which are parent of each other must be in 2 different "layers"                                $e1
  2 nodes which are in the same "layer" are not parent of each other                                    $e2
  if the nodes don't have circular dependencies then the layers don't have circular dependencies either $e3
  each node must be contained in exactly one layer                                                      $e4
                                                                                                        """

  def e1 = prop { nodes: Seq[N] =>
    val sorted = sort(nodes)

    val parentPairs = distinctPairs(sorted.flatten, sorted.flatten).
      filter(areStrictParent)

    parentPairs must contain(not(inSameLayer(sorted))).forall
  }.set(maxSize = 6)

  def e2 = prop { nodes: Seq[N] =>
    sort(nodes).forall { layer =>
      distinctPairs(layer, layer) must contain(not(isStrictParent)).forall
    }
  }.set(maxSize = 6)

  def e3 = prop { nodes: Seq[N] =>
    val sorted = sort(nodes)
    val layerPairs = distinctPairs(sorted, sorted)
    val interLayerPairs = layerPairs.flatMap { case (layer1, layer2) => distinctPairs(layer1, layer2) }
    interLayerPairs must not contain circularDependency
  }.set(maxSize = 6)

  def e4 = prop { nodes: Seq[N] =>
    val sorted = sort(nodes)
    nodes must contain(exactlyInOneLayer(sorted)).forall
  }.set(maxSize = 6)

  /**
   * NODES DEFINITIONS
   */
  case class N(id: Int = N.newId, previous: Seq[N] = Seq()) {
    override def toString = {
      if (previous.isEmpty) s"""N($id)"""
      else s"""N($id)${previous.map(_.id).mkString("[", ", ", "]")}"""
    }
  }
  object N {
    var id: Int = 0
    def newId = { id += 1; id }
  }

  type Node = N
  implicit val successorRelationship: Successor[N] = new Successor[N] { def previous(n: N) = n.previous }
  implicit val isSameRelationship: IsSame[N] = new IsSame[N] { def isSame(n1: N, n2: N) = n1.id == n2.id }

  /**
   * MATCHERS
   */
  def isStrictParent: Matcher[(N, N)] = (n1n2: (N, N)) =>
    (areStrictParent(n1n2), s"$n1n2 are not strict parents")

  def inSameLayer(sorted: Seq[Seq[N]]): Matcher[(N, N)] = (n1n2: (N, N)) =>
    (layer(sorted, n1n2._1) == layer(sorted, n1n2._2), s"$n1n2 are not in the same layer")

  def exactlyInOneLayer(sorted: Seq[Seq[N]]): Matcher[N] = (n: N) =>
    (sorted.filter(_.contains(n)).size == 1, s"$n is not contained in exactly one layer")

  def layer(sorted: Seq[Seq[N]], n: N) = sorted.find(_.contains(n))

  def circularDependency: Matcher[(N, N)] = (n1n2: (N, N)) =>
    (isCircular(n1n2), s"$n1n2 has a circular dependency")

  def isCircular(n1n2: (N, N)) = {
    val (n1, n2) = n1n2
    descendents(n1).contains(n2) && descendents(n2).contains(n1)
  }

  def distinctPairs[A <: AnyRef](seq1: Seq[A], seq2: Seq[A]) =
    ^(seq1.toStream, seq2.toStream)((_,_)).filterNot { case (a1, a2) => a1 eq a2 }

  def areStrictParent(n1n2: (N, N)) = {
    val (n1, n2) = n1n2
    !isSameRelationship.isSame(n1, n2) &&
      (descendents(n1).contains(n2) ||
       descendents(n2).contains(n1))
  }

  /**
   * DATA GENERATION
   */
  implicit def arbitraryNodes: Arbitrary[Seq[N]] = Arbitrary(genNodes)

  def genNodes: Gen[Seq[N]] = Gen.listOf(genNode)
  def genNode: Gen[Node] = for {
    n  <- Gen.frequency((4, Gen.const(0)), (2, Gen.const(1)), (1, Gen.const(2)))
    ns <- Gen.listOfN(n, genNode)
  } yield N(previous = ns)
}
