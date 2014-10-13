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

object P {
}

sealed trait P {
  def fold[X](
    root: => X
  , relative: => X
  , component: (P, FileName) => X
  ): X = this match {
    case Root =>
      root
    case Relative =>
      relative
    case Component(d, n) =>
      component(d, n)
  }

  def isRoot =
    fold(true, false, (_, _) => false)

  def isRelativeRoot =
    fold(false, true, (_, _) => false)

  def dirname: P =
    fold(Root, Relative, (d, n) => d)

  def parent: Option[P] =
    fold(None, None, (d, n) => Some(d))

  def basename: Option[FileName] =
    fold(None, None, (d, n) => Some(n))

  /** @return the path for this file as a / separated string */
  def path: String =
    fold("/", "", (d, p) =>
      d.fold("/", "", (_, _) =>
        s"${d.path}/") + p.name)

  /** @return the path for this file as a / separated string, with a final / */
  def dirPath: String =
    if (isRoot) path else path + "/"

  def </>[B](other: P): P =
    (this, other) match {
      case (Root, Root) =>
        Root
      case (Root, Relative) =>
        Root
      case (Relative, Root) =>
        Relative
      case (Relative, Relative) =>
        Relative
      case (Component(_, _), Root) =>
        Root
      case (Component(_, _), Relative) =>
        this
      case (x, Component(d, n)) =>
        x </> d </> n
    }

  def </>(other: FileName): P =
    fold(
      Component(Root, other)
    , Component(Relative, other)
    , (_, _) => Component(this, other)
    )

  /** @return a File for this path */
  def toFile: File =
    new File(path)

  /** @return the portion of a file path that is relative to another */
  def relativeTo(other: P): Option[P] =
    fold(
      None
    , None
    , (d, p) => if (d == other) (Relative </> p).some
                else            d.relativeTo(other).map(_ </> p))

  /** @return true if the file path is absolute */
  def isAbsolute: Boolean =
    fold(true, false, (d, _) => d.isAbsolute)

  /** @return true if this file path is relative */
  def isRelative: Boolean =
    !isAbsolute

  /** @return all the names of this path */
  def names: List[FileName] =
    fold(Vector(), Vector(), (d, n) => d.names :+ n).toList

  /** @return all the names of this path */
  def components: List[String] =
    names.map(_.name)
}

case object Root extends P
case object Relative extends P
case class Component(dir: P, name: FileName) extends P
