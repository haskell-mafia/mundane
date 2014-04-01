package com.ambiata.mundane
package io

import java.io._
import com.ambiata.mundane.control._
import scalaz.effect.IO

// CONSIDER: Split into directory / files.
case class FilePath(path: String) {
  def isEmpty: Boolean =
    path.isEmpty

  def basename: FilePath =
    FilePath(toFile.getName)

  def dirname: FilePath =
    parent.getOrElse(FilePath("."))

  def rootname: FilePath =
    parent.map(_.rootname).getOrElse(basename)

  def absolute: FilePath =
    FilePath(toFile.getAbsolutePath)

  def toFile: File =
    new File(path)

  def parent: Option[FilePath] =
    Option(toFile.getParentFile).map(f => FilePath(f.getPath))

  def toOutputStream: ResultT[IO, OutputStream] =
    ResultT.safe { new FileOutputStream(path) }

  def toInputStream: ResultT[IO, InputStream] =
    ResultT.safe { new FileInputStream(path) }

  def </>(path: FilePath): FilePath =
    FilePath(new java.io.File(toFile, path.path).getPath)

  def </>(path: String): FilePath =
    </>(FilePath(path))

  /**
   * @return the portion of a file path that is relative to another
   */
  def relativeTo(other: FilePath): FilePath =
    FilePath {
      if (path.startsWith(other.path)) {
        val relative = path.replace(other.path, "")
        if (relative.startsWith("/")) relative.drop(1)
        else                          relative
      }
      else path
    }

  def fromRoot: FilePath =
    relativeTo(rootname)

  override def toString =
    path
}

case class FilePathSyntax(raw: String) {
  def toFilePath =
    FilePath(raw)

  def </>(path: FilePath): FilePath =
    toFilePath </> path

  def </>(path: String): FilePath =
    toFilePath </> path
}
