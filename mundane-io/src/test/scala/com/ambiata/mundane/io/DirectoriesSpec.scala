package com.ambiata.mundane.io

import com.ambiata.mundane.io.Arbitraries._
import com.ambiata.mundane.testing.RIOMatcher._
import com.ambiata.mundane.path._

import org.specs2._
import org.scalacheck._
import org.specs2.matcher.ResultMatchers
import scalaz._, Scalaz._
import MemoryConversions._

class DirectoriesSpec extends Specification with ScalaCheck with ResultMatchers { def is = s2"""

Directories
-----------

  should list all files                     list
  should recursively delete all files       delete
  should determine if directory exists      exists
  should determine if directory not exists  notExists
  should determine the size of a directory  size

"""
/*
  def list = prop((tree: FileTree, local: LocalTemporary) => for {
    b <- local.directory
    _ <- tree.create(b)
    l <- Directories.list(b)
  } yield sort(l) ==== sort(tree.files(b)))

  def delete = prop((tree: FileTree, local: LocalTemporary) => for {
    b <- local.directory
    _ <- tree.create(b)
    _ <- Directories.delete(b)
    e <- Directories.exists(b)
  } yield e ==== false)

  def exists = prop((local: LocalTemporary) => for {
    p <- local.directory
    _ <- Directories.mkdirs(p)
    e <- Directories.exists(p)
  } yield e ==== true)

  def notExists = prop((local: LocalTemporary) => for {
    p <- local.directory
    e <- Directories.exists(p)
  } yield e ==== false)

  def sort(files: List[LocalFile]): List[LocalFile] =
    files.sortBy(_.path)

  def size = prop((tree: FileTree, local: LocalTemporary) => for {
    base          <- local.directory
    _             <- tree.create(base)
    files         <- Directories.list(base)
    fileSizes     = files.map(_.toFile.length).suml
    directorySize <- Directories.size(base)
  } yield fileSizes.bytes ==== directorySize)
 */
}
