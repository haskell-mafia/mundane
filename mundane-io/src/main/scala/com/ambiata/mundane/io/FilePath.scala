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
    FilePath(Option(toFile.getParentFile).map(_.getPath).getOrElse("."))

  def absolute: FilePath =
    FilePath(toFile.getAbsolutePath)

  def toFile: File =
    new File(path)

  def toOutputStream: ResultT[IO, OutputStream] =
    ResultT.safe { new FileOutputStream(path) }

  def toInputStream: ResultT[IO, InputStream] =
    ResultT.safe { new FileInputStream(path) }

  def </>(path: FilePath): FilePath =
    FilePath(new java.io.File(toFile, path.path).getPath)

  def </>(path: String): FilePath =
    </>(FilePath(path))
}

case class FilePathSyntax(raw: String) {
  def toFilePath =
    FilePath(raw)

  def </>(path: FilePath): FilePath =
    toFilePath </> path

  def </>(path: String): FilePath =
    toFilePath </> path
}
