package com.ambiata.mundane.io

import java.io.File
import scalaz._, Scalaz._

sealed trait Path {
  def fold[X](
    root: => X
  , relative: => X
  , component: (Path, FileName) => X
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

  def dirname: Path =
    fold(Root, Relative, (d, n) => d)

  def parent: Option[Path] =
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

  def </>[B](other: Path): Path =
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

  def </>(other: FileName): Path =
    fold(
      Component(Root, other)
    , Component(Relative, other)
    , (_, _) => Component(this, other)
    )

  /** @return a File for this path */
  def toFile: File =
    new File(path)

  /** @return the portion of a file path that is relative to another */
  def relativeTo(other: Path): Option[Path] =
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

object Path {
  def apply(s: String): Path =
   s.split("/").toList match {
      case "" :: Nil =>
        Relative
      case "" :: parts =>
        fromList(Root, parts.filter(_.nonEmpty).map(FileName.unsafe))
      case parts =>
        fromList(Relative, parts.filter(_.nonEmpty).map(FileName.unsafe))
    }

 def fromList(dir: Path, parts: List[FileName]): Path =
    parts.foldLeft(dir)((acc, el) => acc </> el)

  def fromFile(f: File): Path =
    Option(f.getParentFile) match {
      case None =>
        Component(if (f.isAbsolute) Root else Relative, FileName.unsafe(f.getName))
      case Some(p) =>
        fromFile(f) </> f.getName
    }
}

case object Root extends Path
case object Relative extends Path
case class Component(dir: Path, name: FileName) extends Path
