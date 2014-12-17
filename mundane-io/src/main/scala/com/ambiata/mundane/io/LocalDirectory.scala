package com.ambiata.mundane.io

import com.ambiata.mundane.control._
import com.ambiata.mundane.path._
import java.io._
import java.net.URI
import scalaz._, Scalaz._, effect._

/**
 * 'LocalDirectory' represents a directory that exists on a local file system
 */
class LocalDirectory private (val path: Path) extends AnyVal {
  def toLocalPath: LocalPath =
    LocalPath(path)

  def toFile: File =
    new File(path.path)

  def delete: RIO[Unit] =
    listPathsRecursively >>= (_.traverse(_.deleteIt).void)

  def parent: Option[LocalDirectory] =
    path.parent match {
      case Some(p) =>
        new LocalDirectory(p).some
      case None =>
        none
    }

  def move(destination: LocalPath): RIO[Unit] =
    RIO.safe(toFile.renameTo(destination.toFile))

  def moveTo(destination: LocalDirectory): RIO[Unit] =
    path.basename match {
      case None =>
        RIO.fail("Source is a top level directory, can't move")
      case Some(filename) =>
        RIO.safe(toFile.renameTo((destination.path </ filename).toFile))
    }

  def copy(destination: LocalPath): RIO[Unit] =
    listRecursivelyRelativeTo.flatMap(_.traverseU(p =>
      p._1.copy(destination / p._2.path)
    )).void

  def copyTo(destination: LocalDirectory): RIO[Unit] =
    path.basename match {
      case None =>
        RIO.fail("Source is a top level directory, can't copy")
      case Some(filename) =>
        listRecursivelyRelativeTo.flatMap(_.traverseU(p =>
          p._1.copy((destination.toLocalPath | filename) / p._2.path)
        )).void
    }

  def listFilesRelativeTo: RIO[List[(LocalFile, LocalPath)]] =
    listFiles.flatMap(_.traverseU(f =>
      RIO.fromOption[LocalPath](f.toLocalPath.rebaseTo(toLocalPath),
        "Invariant failure, this is likely a bug: https://github.com/ambiata/mundane/issues").map(f -> _)
    ))

  def listRecursivelyRelativeTo: RIO[List[(LocalFile, LocalPath)]] =
    listFilesRecursively.flatMap(_.traverseU(f =>
      RIO.fromOption[LocalPath](f.toLocalPath.rebaseTo(toLocalPath),
        "Invariant failure, this is likely a bug: https://github.com/ambiata/mundane/issues").map(f -> _)
    ))

  def listFilesRecursively: RIO[List[LocalFile]] =
    RIO.safe[List[Path]]({
      def loop(p: Path): List[Path] = {
        val f = Option(new File(p.path).listFiles).cata(_.toList, List())
        f.flatMap({ f =>
          if (f.isDirectory) loop(p </ Component.unsafe(f.getName))
          else               List(p </ Component.unsafe(f.getName))
        })
      }
      loop(path)
    }) >>= (_.traverse(LocalPath(_).determineFile))

  def listDirectoriesRecursively: RIO[List[LocalDirectory]] =
    RIO.safe[List[Path]]({
      def loop(p: Path): List[Path] = {
        val f = Option(new File(p.path).listFiles).cata(_.toList, List())
        f.flatMap({ f => {
          val path = p </ Component.unsafe(f.getName)
          if (f.isDirectory) List(path) ++ loop(path)
          else               List()
        }})
      }
      loop(path)
    }) >>= (_.traverse(LocalPath(_).determineDirectory))

  def listPathsRecursively: RIO[List[LocalPath]] =
    RIO.safe[List[LocalPath]]({
      def loop(p: LocalPath): List[LocalPath] = {
        val f = Option(p.toFile.listFiles).cata(_.toList, List())
        f.flatMap({ f => {
          val path = p | Component.unsafe(f.getName)
          if (f.isDirectory) List(path) ++ loop(path)
          else               List(path)
        }})
      }
      loop(toLocalPath)
    })

  def listDirectories: RIO[List[LocalDirectory]] =
    RIO.safe[List[Path]]({
      Option(new File(path.path).listFiles).cata(_.toList, List()).flatMap(f => {
        if (f.isDirectory) List(path </- f.getName)
        else               List()
      })
    }) >>= (_.traverseU(LocalPath(_).determineDirectory))

  def listFiles: RIO[List[LocalFile]] =
    RIO.safe[List[Path]]({
      Option(path.toFile.listFiles).cata(_.toList, List()).flatMap(f => {
        if (f.isFile) List(path </- f.getName)
        else          List()
      })
    }) >>= (_.traverseU(LocalPath(_).determineFile))

  def listPaths: RIO[List[LocalPath]] =
    RIO.safe[List[LocalPath]]({
        Option(path.toFile.listFiles).cata(_.toList, List()).map(f => toLocalPath /- f.getName)
      })

}

object LocalDirectory {
  def Root: Path =
    com.ambiata.mundane.path.Root

  def Relative: Path =
    com.ambiata.mundane.path.Relative

  def fromFile(f: File): LocalDirectory =
    unsafe(f.getPath)

  def fromString(s: String): Option[LocalDirectory] =
    s.split("/").toList match {
      case "" :: Nil =>
        None
      case "" :: parts =>
        parts.traverse(Component.create).map(fromList(Root, _))
      case parts =>
        parts.traverse(Component.create).map(fromList(Relative, _))
    }

  def fromList(dir: Path, parts: List[Component]): LocalDirectory =
    new LocalDirectory(parts.foldLeft(dir)((acc, el) => acc </ el))

  def unsafe(s: String): LocalDirectory =
    fromString(s).getOrElse(sys.error("LocalDirectory.unsafe on an invalid string."))

  def fromURI(s: URI): Option[LocalDirectory] =
    fromString(s.getPath)
}
