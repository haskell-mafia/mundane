package com.ambiata.mundane.io

import com.ambiata.mundane.control._
import com.ambiata.mundane.path._
import java.io._
import java.net.URI
import scalaz._, Scalaz._

/**
 * 'LocalDirectory' represents a directory that exists on a local file system
 */
class LocalDirectory private (val path: Path) extends AnyVal {
  def toLocalPath: LocalPath =
    LocalPath(path)

  def toPath: Path =
    path

  def toFile: File =
    new File(path.path)

  def exists: RIO[Boolean] =
    RIO.safe[Boolean](toFile.isDirectory)

  def onExists[A](success: => RIO[A], missing: => RIO[A]): RIO[A] =
    exists >>= (e =>
      if (e) success
      else   missing
    )

  def whenExists(thunk: => RIO[Unit]): RIO[Unit] =
    onExists(thunk, RIO.unit)

  def doesExist[A](error: String, thunk: => RIO[A]): RIO[A] =
    onExists(thunk, RIO.failIO(error))

  def doesNotExist[A](error: String, thunk: => RIO[A]): RIO[A] =
    onExists(RIO.failIO(error), thunk)

/**
  This needs to list recursively and delete from the leaf up.
  i.e.
  .
  └── a
      ├── b
      │   ├── bar
      │   └── c
      └── foo
  Delete in this order: (bar -> c -> b -> foo -> a)
  */
  def delete: RIO[Unit] = for {
    f <- RIO.safe(Option(path.toFile.listFiles).cata(_.toList, List()))
    _ <- f.traverse(p => (toLocalPath /- p.getName).delete)
    _ <- RIO.safe(toFile.delete)
  } yield ()

  def parent: Option[LocalDirectory] =
    path.parent.map(new LocalDirectory(_))

  def move(destination: LocalPath): RIO[LocalDirectory] =
    path.basename match {
      case None =>
        RIO.fail("Source is a top level directory, can't move")
      case Some(_) =>
        doesExist(s"Source directory does not exists. LocalDirectory($path)",
          destination.doesNotExist(s"A file/directory exists in the target location $destination. Can not move source directory LocalDirectory($path).",
            RIO.safe({
              val dest = destination.toFile
              destination.dirname.toFile.mkdirs
              toFile.renameTo(dest)
            }).as(LocalDirectory.unsafe(destination.path.path))))
    }

  def moveWithMode(destination: LocalPath, mode: TargetMode): RIO[LocalDirectory] =
    mode.fold(doesExist(s"Source directory does not exists. LocalDirectory($path)",
      RIO.safe({
        val dest = destination.toFile
        destination.dirname.toFile.mkdirs
        toFile.renameTo(dest)
      }).as(LocalDirectory.unsafe(destination.path.path))), move(destination))

  def moveTo(destination: LocalDirectory): RIO[LocalDirectory] =
    path.basename match {
      case None =>
        RIO.fail("Source is a top level directory, can't move")
      case Some(filename) =>
        move(destination.toLocalPath | filename)
    }

  def listFilesRelativeTo: RIO[List[(LocalFile, LocalPath)]] =
    listFiles.flatMap(_.traverseU(f =>
      RIO.fromOption[LocalPath](f.toLocalPath.rebaseTo(toLocalPath),
        "Invariant failure, this is likely a bug: https://github.com/ambiata/mundane/issues").map(f -> _)))

  def listFilesRecursivelyRelativeTo: RIO[List[(LocalFile, LocalPath)]] =
    listFilesRecursively.flatMap(_.traverseU(f =>
      RIO.fromOption[LocalPath](f.toLocalPath.rebaseTo(toLocalPath),
        "Invariant failure, this is likely a bug: https://github.com/ambiata/mundane/issues").map(f -> _)))

  def listFilesRecursively: RIO[List[LocalFile]] =
    RIO.safe[List[Path]]({
      def loop(p: Path): List[Path] = {
        val f = Option(new File(p.path).listFiles).cata(_.toList, List())
        f.flatMap({ f =>
          if (f.isDirectory) loop(p | Component.unsafe(f.getName))
          else               List(p | Component.unsafe(f.getName))
        })
      }
      loop(path)
    }) >>= (_.traverse(LocalPath(_).determineFile))

  def listDirectoriesRecursively: RIO[List[LocalDirectory]] =
    RIO.safe[List[Path]]({
      def loop(p: Path): List[Path] = {
        val f = Option(new File(p.path).listFiles).cata(_.toList, List())
        f.flatMap({ f => {
          val path = p | Component.unsafe(f.getName)
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
        if (f.isDirectory) List(path /- f.getName)
        else               List()
      })
    }) >>= (_.traverseU(LocalPath(_).determineDirectory))

  def listFiles: RIO[List[LocalFile]] =
    RIO.safe[List[Path]]({
      Option(path.toFile.listFiles).cata(_.toList, List()).flatMap(f => {
        if (f.isFile) List(path /- f.getName)
        else          List()
      })
    }) >>= (_.traverseU(LocalPath(_).determineFile))

  def listPaths: RIO[List[LocalPath]] =
    RIO.safe[List[LocalPath]]({
        Option(path.toFile.listFiles).cata(_.toList, List()).map(f => toLocalPath /- f.getName)
      })

}

object LocalDirectory {
  def fromFile(f: File): RIO[LocalDirectory] =
    LocalPath.fromFile(f).flatMap(_.determineDirectory)

  def fromString(s: String): RIO[Option[LocalDirectory]] =
    LocalPath.fromString(s).determinefWithPure(_ => none, _.some, none)

  def fromList(dir: Path, parts: List[Component]): RIO[Option[LocalDirectory]] =
    LocalPath.fromList(dir, parts).determinefWithPure(_ => none, _.some, none)

  def fromURI(s: URI): RIO[Option[LocalDirectory]] =
    LocalPath.fromURI(s).map(_.determineDirectory).sequence

  private[io] def unsafe(s: String): LocalDirectory =
    new LocalDirectory(Path(s))

  def filterHidden(l: List[LocalDirectory]): List[LocalDirectory] =
    l.filter(f => !List(".", "_").exists(c => f.toPath.basename.exists(_.name.startsWith(c))))

  implicit def LocalDirectoryOrder: Order[LocalDirectory] =
    Order.order((x, y) => x.path.?|?(y.path))

  implicit def LocalDirectoryOrdering: scala.Ordering[LocalDirectory] =
    LocalDirectoryOrder.toScalaOrdering
}
