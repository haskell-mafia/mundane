package com.ambiata.mundane.io

import com.ambiata.mundane.path._
import java.io._
import java.net.URI
import scalaz._, Scalaz._

//case class LocalDirectory(path: Path)

object LocalDirectory {
  def Root: LocalDirectory =
    com.ambiata.mundane.path.Root

  def Relative: LocalDirectory =
    com.ambiata.mundane.path.Relative

  def fromFile(f: File): LocalDirectory =
    unsafe(f.getPath)

  def fromString(s: String): Option[LocalDirectory] =
    s.split("/").toList match {
      case "" :: Nil =>
        None
      case "" :: parts =>
        parts.traverse(FileName.create).map(LocalFile.fromList(Root, _))
      case parts =>
        parts.traverse(FileName.create).map(LocalFile.fromList(Relative, _))
    }

  def unsafe(s: String): LocalDirectory =
    fromString(s).getOrElse(sys.error("LocalDirectory.unsafe on an invalid string."))

  def fromURI(s: URI): Option[LocalDirectory] =
    fromString(s.getPath)
}
