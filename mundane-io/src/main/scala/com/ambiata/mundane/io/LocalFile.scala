package com.ambiata.mundane.io

import com.ambiata.mundane.path._
import java.io._
import java.net.URI
import java.util.UUID
import scalaz._, Scalaz._

object LocalFile {
  def fromFile(f: File): Path =
    unsafe(f.getPath)

  def fromString(s: String): Option[Path] =
    s.split("/").toList match {
      case "" :: Nil =>
        None
      case "" :: parts =>
        parts.traverse(FileName.create).map(fromList(Root, _))
      case parts =>
        parts.traverse(FileName.create).map(fromList(Relative, _))
    }

  def fromList(dir: Path, parts: List[FileName]): Path =
    parts.foldLeft(dir)((acc, el) => acc </ el)

  def fromURI(s: URI): Option[LocalFile] =
    fromString(s.getPath)

  def unsafe(s: String): LocalFile =
    fromString(s).getOrElse(sys.error("LocalFile.unsafe on an invalid string."))
}
