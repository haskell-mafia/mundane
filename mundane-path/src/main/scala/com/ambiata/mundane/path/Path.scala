package com.ambiata.mundane.path

import java.io.File
import scalaz._, Scalaz._

/**
 * A path data type, represents an "Absolute" or "Relative" path onto
 * some posix-like file system.
 *
 * The data-type is defined inductively, with a base case indicating
 * whether the path is relative to the root of the file system, or
 * some relative point, and a recursive case for each filename component.
 *
 * Clients are free to choose the intention of "Relative", i.e. on a
 * posix file system it may be the `$CWD` or similar.
 */
sealed trait Path {
  /** Fold over components of this path. Equivalent to pattern match. */
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

  /** Is this the Root base case. Note, this is true iff this is actually the
      Root data type, and should not be used to detemine if this is an absolute
      path. */
  def isRoot =
    fold(true, false, (_, _) => false)

  /** Is this the Relative base case. Note, this is true iff this is actually
      the Relative data type and should not be used to detemine if this is a
      relative path. */
  def isRelativeRoot =
    fold(false, true, (_, _) => false)

  /** Return a new path with the top filename component stripped off.
      Behaves as per posix dirname(3). Calling this on Root or Relative
      is just identity. */
  def dirname: Path =
    fold(Root, Relative, (d, n) => d)

  /** Iff this path has a component, returns 'Some' new path with the top filename
      component stripped off, otherwise returns 'None'. Behaves as per 'dirname'
      except for the base cases. */
  def parent: Option[Path] =
    fold(None, None, (d, n) => Some(d))

  /** Iff this path has a component, returns 'Some' with the top filename component
      of this 'Path'. This behaves as per posix basename(3) for the 'Some' case. */
  def basename: Option[FileName] =
    fold(None, None, (d, n) => Some(n))

  /** Is the provided Path 'p' a prefix of 'this' Path? */
  def startsWith(p: Path): Boolean =
    if (p.isRoot)
      isAbsolute
    else if (p.isRelativeRoot)
      isRelative
    else
      path.startsWith(p.path)

  /** Is this path is a prefix of specified Path 'p'. This is 'startsWith' with the arguments flipped. */
  def isPrefixOf(p: Path): Boolean =
    p.startsWith(this)

  /** The rendered string represententation using '/' as a path separator.
      This string will start with a leading separator iff the path 'isAbsolute'. */
  def path: String =
    pathWith("/")

  /** The rendered string represententation using the specified path separator.
      This string will start with a leading separator iff the path 'isAbsolute'. */
  def pathWith(separator: String) : String =
    fold(separator, "", (d, p) =>
      d.fold(separator, "", (_, _) =>
        s"${d.pathWith(separator)}${separator}") + p.name)

  /** Append the 'other' path to 'this' path. In general, the behaviour of this
      matches what you expect from traditional high-level path join operations (
      python `os.path.join`, ruby `File.join`, java `new File(this, other)`),
      however there are a number of conflicting cases that are not consistent
      across any libary. The behaviour of Path in these situations is:

          this  | other | result
          ----------------------
          Root  | Root  | Root
          Root  | Rel   | Root
          Rel   | Root  | Root
          Rel   | Rel   | Rel
          (abs) | (abs) | other (abs)
          (abs) | (rel) | this + other (abs)
          (rel) | (abs) | other (abs)
          (rel) | (rel) | this + other (rel)

      Where:
       - (abs) is some arbitrary absolute path
       - (rel) is some arbitrary relative path

      Note this can be captured by the general hueristics:
       - anytime 'other' is absolute, the result is just 'other'.
         - otherwise it is a combination of 'this' and 'other'.
       - if any of the components are absolute, the result will be absolute
         - otherwise, all components and the result are relative.

      This implementation is most consistent with the python
      implementation, but not prescriptively so. The desired goal is
      to have the implementation, that creates the fewest incorrect
      paths (i.e. paths that when combined do not logically fall
      within either of the two parts).

      Also note the strong implication that combining two paths is
      associative.
  */
  def </>[B](other: Path): Path =
    (this, other) match {
      case (Root, Root) =>
        Root
      case (Root, Relative) =>
        Root
      case (Relative, Root) =>
        Root
      case (Relative, Relative) =>
        Relative
      case (Component(_, _), Root) =>
        Root
      case (Component(_, _), Relative) =>
        this
      case (x, Component(d, n)) =>
        x </> d </ n
    }

  /** Named alias for '</>' operator. See extended description on '</>'. */
  def join(path: Path): Path =
    </>(path)

  /** Append the specified 'FileName' to this 'Path'. The following invariants
      always hold:
        - extending this path with a filename and the calling dirname is a no-op
        - extending this path with a filename and the calling basename always
          gives the filename  */
  def </(other: FileName): Path =
    fold(
      Component(Root, other)
    , Component(Relative, other)
    , (_, _) => Component(this, other)
    )

  /** Named alias for '</' operator. See extended description on '</'. */
  def extend(other: FileName): Path =
    </(other)

  /** Construct a 'java.io.File' corresponding to this path. */
  def toFile: File =
    new File(path)

  /** Is this file path absolute? i.e. is the base case the 'Root' element.
      This is the inverse of isRelative. */
  def isAbsolute: Boolean =
    fold(true, false, (d, _) => d.isAbsolute)

  /** Is this file path relative i.e. is the base case the 'Relative' element.
      This is the inverse of isAbsolute. */
  def isRelative: Boolean =
    !isAbsolute

  /** Return all the 'FileName' components of this 'Path'.

      WARNING: this is a lossy operation, it should not be used
      in isolation. Any use of this without _isAbsolute_ or
      _isRelative_ is almost certainly a bug. */
  def names: List[FileName] =
    fold(Vector(), Vector(), (d, n) => d.names :+ n).toList

  /** Return all the 'String' names of this 'Path'. This is a
      weaker typed 'name'.

      WARNING: this is a lossy operation, it should not be used
      in isolation. Any use of this without _isAbsolute_ or
      _isRelative_ is almost certainly a bug. */
  def components: List[String] =
    names.map(_.name)

  /** Rebase this path to another path, effectively stripping the
      'other' prefix from 'this' path. This will return 'Some'
      path only if other is a prefix of this path, otherwise it
      will return 'None'. No normalization is done on paths. */
  def rebaseTo(other: Path): Option[Path] =
    if (this == other)
      Some(Relative)
    else
      fold(
        None
      , None
      , (d, p) => if (d == other) (Relative </ p).some
                  else            d.rebaseTo(other).map(_ </ p))
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
    parts.foldLeft(dir)((acc, el) => acc </ el)

  def fromFile(f: File): Path =
    Option(f.getParentFile) match {
      case None =>
        val base = if (f.isAbsolute) Root else Relative
        if (f.getName == "") base else Component(base, FileName.unsafe(f.getName))
      case Some(p) =>
        fromFile(p) </ FileName.unsafe(f.getName)
    }

  def Parent =
    FileName.unsafe("..")

  def Current =
    FileName.unsafe(".")
}

case object Root extends Path
case object Relative extends Path
case class Component(dir: Path, name: FileName) extends Path
