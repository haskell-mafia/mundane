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
  def path: String = dirname.path+"/"+basename.name

  /** @return a File for this path */
  def toFile: File = new File(path)

  /** @return the portion of a file path that is relative to another */
  def relativeTo(other: DirPath): FilePath =
    copy(dirname = dirname.relativeTo(other))

  /** @return return the portion of the file path that starts from the rootname */
  def fromRoot: FilePath = relativeTo(rootname)

  def toMultipart = new MultipartFilePath(this)

  override def toString = path
}

/**
 * Representation of a directory in some sort of tree-based file system (S3, Hdfs, local,...)
 *
 * It is a list of FileNames and we can append other DirPaths or FilePaths to it
 *
 * If the list is empty, this means we are at the root (the equivalent of / on a Unix file system)
 *
 */
case class DirPath(dirs: List[FileName]) {
  /** @return either the parent directory or the root if we already are at the root */
  def dirname: DirPath  = parent.getOrElse(this)

  /** @return the last file name of the list or . if the list is empty */
  def basename: FileName = dirs.lastOption.getOrElse(FileName.unsafe("."))

  /** @return the dir path for the first name in the list */
  def rootname: DirPath = copy(dirs = dirs.take(1))

  /** @return the parent directory for this directory, none if we are at the root */
  def parent: Option[DirPath] =
    dirs match {
      case Nil       => None
      case h :: tail => Some(copy(dirs = dirs.dropRight(1)))
    }

  /** @return the path for this file as a / separated string */
  def path: String = dirs.map(_.name).toList.mkString("/")

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
      case (Nil, _)                      => this
      case (h :: t, Nil)                 => this
      case (h :: t, h1 :: t1) if h == h1 => copy(dirs = t).relativeTo(other.copy(dirs = t1))
      case _                             => this
    }

  /** @return the DirPath starting from the rootname */
  def fromRoot: DirPath = relativeTo(rootname)

  /** @return interpret this DirPath as a FilePath, which might be /. if this DirPath is Root */
  def toFilePath: FilePath = FilePath(dirname, basename)

  def toMultipart = MultipartFilePath(toFilePath)

  override def toString = path
}

/**
 * A multipart file is either:
 *
 *  - a single file contained in a parent directory
 *  - a directory containing several files
 *
 *  Internally we use a FilePath to model both situations
 */
case class MultipartFilePath(filePath: FilePath) {
  def dirname: DirPath        = filePath.dirname
  def rootname: DirPath       = filePath.rootname

  def path: String = filePath.path
  def toFile: File = filePath.toFile

  def relativeTo(other: DirPath): FilePath = filePath.relativeTo(other)
  def fromRoot: FilePath = filePath.fromRoot

  override def toString = path
}

object MultipartFilePath {
  def apply(n: FileName): MultipartFilePath = MultipartFilePath(FilePath(n))

  def unsafe(s: String): MultipartFilePath =
    unsafe(new File(s))

  def unsafe(f: File): MultipartFilePath =
    MultipartFilePath(FilePath.unsafe(f))

}

object FilePath {
  def apply(n: FileName): FilePath = FilePath(DirPath.Root, n)

  def unsafe(s: String): FilePath = unsafe(new File(s))
  def unsafe(f: File): FilePath   = unsafe(f.toURI)
  def unsafe(uri: URI): FilePath  = DirPath.unsafe(uri).toFilePath

}

object DirPath {
  def apply(n: FileName): DirPath = apply(List(n))

  def unsafe(s: String): DirPath =
    DirPath(removeScheme(s).split("/").map(FileName.unsafe).toList)

  def unsafe(f: File): DirPath =
    unsafe(f.getPath)

  def unsafe(uri: URI): DirPath =
    unsafe(uri.toString)

  private def removeScheme(s: String): String =
    Seq("hdfs://", "s3://", "file://", "file:").foldLeft(s) { (res, cur) => res.replace(cur, "") }

  val Root = DirPath(dirs = List())
}

class DirPathSyntax(name: FileName) {
  def </>(other: DirPath) : DirPath = DirPath(name +: other.dirs)
  def </>(other: FilePath): FilePath = FilePath(DirPath(name +: other.dirname.dirs), other.basename)
  def </>(other: FileName): DirPath  = DirPath(name) </> other
  def <|>(other: FileName): FilePath = DirPath(name) <|> other
}

