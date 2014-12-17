package com.ambiata.mundane.io

import com.ambiata.mundane.control._
import com.ambiata.mundane.path._
import java.io._
import org.scalacheck._, Arbitrary._
import scalaz._, Scalaz._
import scalaz.effect.IO

sealed trait FileTree {
  def files(base: DirPath): List[FilePath] =
    this match {
      case FileTreeLeaf(label) =>
        List(base </ FileName.unsafe(label))
      case FileTreeDirectory(label, children) =>
        children.flatMap(_.files(base </ FileName.unsafe(label)))
    }

  def dirs(base: DirPath): List[DirPath] =
    this match {
      case FileTreeLeaf(label) =>
        List()
      case FileTreeDirectory(label, children) =>
        val dir = base </ FileName.unsafe(label)
        dir :: children.flatMap(_.dirs(dir))
    }

  def create(base: DirPath): RIO[Unit] =
    this match {
      case FileTreeLeaf(label) =>
        val path = base </ FileName.unsafe(label)
        Files.write(path, s"contents of $label")
      case FileTreeDirectory(label, children) =>
        val path = base </ FileName.unsafe(label)
        Directories.mkdirs(path) >>
          children.traverseU(_.create(path)).void
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
