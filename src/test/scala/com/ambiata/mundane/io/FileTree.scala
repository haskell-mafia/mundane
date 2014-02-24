package com.ambiata.mundane
package io

import java.io._
import org.scalacheck._, Arbitrary._
import scalaz._, Scalaz._
import scalaz.effect.IO

sealed trait FileTree {
  def files(base: File): List[File] =
    this match {
      case FileTreeLeaf(label) =>
        List(new File(base, label))
      case FileTreeDirectory(label, children) =>
        children.flatMap(_.files(new File(base, label)))
    }

  def dirs(base: File): List[File] =
    this match {
      case FileTreeLeaf(label) =>
        List()
      case FileTreeDirectory(label, children) =>
        val dir = new File(base, label)
        dir :: children.flatMap(_.dirs(dir))
    }

  def create(base: File): IO[Unit] =
    this match {
      case FileTreeLeaf(label) =>
        val path = new File(base, label)
        IO { base.mkdirs; Streams.write(new FileOutputStream(path), s"contents of $label") }
      case FileTreeDirectory(label, children) =>
        val path = new File(base, label)
        IO { new File(base, label).mkdirs } >>
          children.traverse(_.create(path)).as(())
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
   , 2 -> Gen.choose(0, Int.MaxValue).map(label => FileTreeLeaf(label.toString))
   ))
}
