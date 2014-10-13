package com.ambiata.mundane
package io

import java.io._
import java.net.URI
import java.util.UUID
import scalaz._, Scalaz._

object FilePath {
  def fromFile(f: File): P =
    unsafe(f.getPath)

  def fromString(s: String): Option[P] =
    s.split("/").toList match {
      case "" :: Nil =>
        None
      case "" :: parts =>
        parts.traverse(FileName.create).map(fromList(Root, _))
      case parts =>
        parts.traverse(FileName.create).map(fromList(Relative, _))
    }

  def fromList(dir: P, parts: List[FileName]): P =
    parts.foldLeft(dir)((acc, el) => acc </> el)

  def fromURI(s: URI): Option[FilePath] =
    fromString(s.getPath)

  def unsafe(s: String): FilePath =
    fromString(s).getOrElse(sys.error("FilePath.unsafe on an invalid string."))
}

object DirPath {
  def Root: DirPath =
    com.ambiata.mundane.io.Root

  def Relative: DirPath =
    com.ambiata.mundane.io.Relative

  def fromFile(f: File): DirPath =
    unsafe(f.getPath)

  def fromString(s: String): Option[DirPath] = {
    s.split("/").toList match {
      case "" :: Nil =>
        None
      case "" :: parts =>
        parts.traverse(FileName.create).map(FilePath.fromList(Root, _))
      case parts =>
        parts.traverse(FileName.create).map(FilePath.fromList(Relative, _))
    }
  }

  def unsafe(s: String): DirPath =
    fromString(s).getOrElse(sys.error("DirPath.unsafe on an invalid string."))

  def fromURI(s: URI): Option[DirPath] =
    fromString(s.getPath)
}
