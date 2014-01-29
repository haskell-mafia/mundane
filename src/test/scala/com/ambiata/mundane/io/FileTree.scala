package com.ambiata.mundane
package io

import java.io._
import org.scalacheck._, Arbitrary._
import scalaz._, Scalaz._
import scalaz.effect.IO

sealed trait FileTree {
  def create(base: File): IO[Unit] = IO {
    this match {
      case FileTreeLeaf(label) =>
        val path = new File(base, label)
        IO { Streams.write(new FileOutputStream(path), s"contents of $label") }
      case FileTreeDirectory(label, children) =>
        val path = new File(base, label)
        IO { new File(base, label).mkdirs } >>
          children.traverse(_.create(path))
    }
  }
}
case class FileTreeLeaf(label: String) extends FileTree
case class FileTreeDirectory(label: String, children: List[FileTree]) extends FileTree

object FileTree {
  implicit def FileTreeArbitrary: Arbitrary[FileTree] =
    Arbitrary(Gen.frequency(
     1 -> (for {
             n        <- Gen.choose(0, 5)
             children <- Gen.listOfN(n, arbitrary[FileTree])
             label    <- arbitrary[Int]
           } yield FileTreeDirectory(label.toString, children))
   , 9 -> arbitrary[Int] map (label => FileTreeLeaf(label.toString))
   ))
}
