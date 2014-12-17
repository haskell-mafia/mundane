package com.ambiata.mundane.io

import com.ambiata.mundane.path._
import java.io._
import java.net.URI
import scalaz._, Scalaz._

//case class DirPath(path: Path)

object DirPath {

  def Root: DirPath =
    com.ambiata.mundane.path.Root

  def Relative: DirPath =
    com.ambiata.mundane.path.Relative

  def fromFile(f: File): DirPath =
    unsafe(f.getPath)

  def fromString(s: String): Option[DirPath] =
    s.split("/").toList match {
      case "" :: Nil =>
        None
      case "" :: parts =>
        parts.traverse(FileName.create).map(FilePath.fromList(Root, _))
      case parts =>
        parts.traverse(FileName.create).map(FilePath.fromList(Relative, _))
    }

  def unsafe(s: String): DirPath =
    fromString(s).getOrElse(sys.error("DirPath.unsafe on an invalid string."))

  def fromURI(s: URI): Option[DirPath] =
    fromString(s.getPath)
}
