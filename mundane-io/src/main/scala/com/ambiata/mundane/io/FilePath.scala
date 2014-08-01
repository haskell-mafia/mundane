package com.ambiata.mundane
package io

import java.io._
import java.net.URI
import scalaz._, Scalaz._

/**
 * Representation of a file in some sort of tree-based file system (S3, Hdfs, local,...)
 *
 * It has a parent directory and a name
 */
case class FilePath(dirname: DirPath, basename: FileName) {
  /** @return the root directory containing this file */
  def rootname: DirPath = dirname.rootname

  /** @return the path for this file as a / separated string */
  def path: String = if (dirname.isRoot) basename.name else dirname.path+"/"+basename.name

  /** @return a File for this path */
  def toFile: File = new File(path)

  /** @return the portion of a file path that is relative to another */
  def relativeTo(other: DirPath): FilePath =
    copy(dirname = dirname.relativeTo(other))

  /** @return return the portion of the file path that starts from the rootname */
  def fromRoot: FilePath = relativeTo(rootname)

  /** @return interpret this FilePath as a DirPath */
  def toDirPath: DirPath = dirname </> basename

}

object FilePath {
  def apply(n: FileName): FilePath = FilePath(DirPath.Root, n)

  def unsafe(s: String): FilePath = DirPath.unsafe(s).toFilePath
  def unsafe(f: File): FilePath   = DirPath.unsafe(f).toFilePath
  def unsafe(uri: URI): FilePath  = DirPath.unsafe(uri).toFilePath

}

/**
 * Representation of a directory in some sort of tree-based file system (S3, Hdfs, local,...)
 *
 * It is a list of FileNames and we can append other DirPaths or FilePaths to it
 *
 * If the list is empty, this means we are at the root (the equivalent of / on a Unix file system)
 *
 */
case class DirPath(dirs: Vector[FileName]) {
  /** @return either the parent directory or the root if we already are at the root */
  def dirname: DirPath  = parent.getOrElse(this)

  /** @return the last file name of the list or . if the list is empty */
  def basename: FileName = dirs.lastOption.getOrElse(FileName.unsafe("."))

  /** @return the dir path for the first name in the list */
  def rootname: DirPath = copy(dirs = dirs.take(1))

  /** @return the parent directory for this directory, none if we are at the root */
  def parent: Option[DirPath] =
    dirs match {
      case h +: tail => Some(copy(dirs = dirs.dropRight(1)))
      case _         => None
    }

  /** @return the path for this file as a / separated string */
  def path: String = if (isRoot) "" else dirs.map(_.name).toList.mkString("/")

  /** @return the path for this file as a / separated string, with a final / */
  def dirPath: String = dirs.map(_.name).toList.mkString("", "/", "/")

  /** @return a File for this path */
  def toFile: File = new File(path)

  /**
   * append another dirpath
   *
   * DirPath.Root plays the role an empty element for this operation
   */
  def </>(other: DirPath): DirPath =
    (this, other) match {
      case (_, DirPath.Root) => this
      case (DirPath.Root, _) => other
      case _                 => copy(dirs = dirs ++ other.dirs)
    }

  /**
   * append a FilePath to this directory
   * @return another FilePath
   */
  def </>(other: FilePath): FilePath =
    FilePath(DirPath(dirs ++ other.dirname.dirs), other.basename)

  /**
   * append a new name to this directory
   * @return a DirPath
   */
  def </>(name: FileName): DirPath  = copy(dirs = dirs :+ name)

  /**
   * append a new name to this directory but
   * @return a FilePath
   */
  def <|>(name: FileName): FilePath = FilePath(this, name)

  /**
   * @return the portion of a dir path that is relative to another
   */
  def relativeTo(other: DirPath): DirPath =
    (dirs, other.dirs) match {
      case (h +: t, h1 +: t1) if h == h1 => copy(dirs = t).relativeTo(other.copy(dirs = t1))
      case _                             => this
    }

  /** @return the DirPath starting from the rootname */
  def fromRoot: DirPath = relativeTo(rootname)

  /** @return interpret this DirPath as a FilePath, which might be /. if this DirPath is Root */
  def toFilePath: FilePath = FilePath(dirname, basename)

  def isRoot = dirs.isEmpty

}

object DirPath {
  def apply(n: FileName): DirPath = apply(Vector(n))

  def unsafe(s: String): DirPath =
    DirPath(removeScheme(s).split("/").filter(_.nonEmpty).map(FileName.unsafe).toVector)

  def unsafe(f: File): DirPath =
    unsafe(f.getPath)

  def unsafe(uri: URI): DirPath =
    unsafe(uri.toString)

  private def removeScheme(s: String): String =
    Seq("hdfs:", "s3:", "file:").foldLeft(s) { (res, cur) => res.replace(cur, "") }

  val Root = DirPath(dirs = Vector())
}

class DirPathSyntax(name: FileName) {
  def </>(other: DirPath) : DirPath  = DirPath(name +: other.dirs)
  def </>(other: FilePath): FilePath = FilePath(DirPath(name +: other.dirname.dirs), other.basename)
  def </>(other: FileName): DirPath  = DirPath(name) </> other
  def <|>(other: FileName): FilePath = DirPath(name) <|> other
}

