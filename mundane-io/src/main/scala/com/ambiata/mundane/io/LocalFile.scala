package com.ambiata.mundane.io

import com.ambiata.mundane.control._
import com.ambiata.mundane.path._
import com.ambiata.mundane.io.LocalFile._
import java.io._
import java.net.URI
import java.util.UUID
import scalaz._, Scalaz._, effect._, Effect._

/**
 * 'LocalFile' represents a file that exists on a local file system
 */
class LocalFile private (val path: Path) extends AnyVal {
  def toLocalPath: LocalPath =
    LocalPath(path)

  def toPath: Path =
    path

  def toFile: File =
    new File(path.path)

// ============= Operations =================

  def exists: RIO[Boolean] = RIO.safe[Boolean] {
    val file = toFile
    file.exists && file.isFile
  }

  def unlessExists[A](error: String, thunk: => RIO[A]): RIO[A] =
    exists >>= (e =>
      if (e) RIO.failIO(error)
      else thunk
    )

  def delete: RIO[Unit] = for {
    e <- exists
  } yield !e || toFile.delete

  def read: RIO[String] =
    readWithEncoding("UTF-8")

  def readWithEncoding(encoding: String): RIO[String] =
    RIO.using(this.toInputStream) { in => Streams.read(in, encoding) }

  def readLines: RIO[Vector[String]] =
    read.map(_.lines.toVector)

  def readUnsafe(f: java.io.InputStream => RIO[Unit]): RIO[Unit] =
    RIO.using(this.toInputStream)(f)

  def readPerLine[A](f: String => RIO[Unit]): RIO[Unit] = // TODO add spec
    RIO.using(this.toInputStream)(in =>
      RIO.io {
        val reader = new java.io.BufferedReader(new java.io.InputStreamReader(in, "UTF-8"))
        var line: String = null
        var result: RIO[Unit] = null
        while ({ line = reader.readLine; line != null && result == null })
          f(line).run.unsafePerformIO match {
            case Ok(_) => ()
            case e @ Error(_) => result = RIO.result[Unit](e)
          }
        }
      )

  def readPerLine[A](empty: => A)(f: (String, A) => A): RIO[A] = // TODO add spec
    RIO.io(empty).flatMap { s =>
      var state = s
      readUnsafe { in => RIO.io {
        val reader = new java.io.BufferedReader(new java.io.InputStreamReader(in, "UTF-8"))
        var line: String = null
        while ({ line = reader.readLine; line != null })
          state = f(line, state)
      }}.as(state)
    }

  def readLinesWithEncoding(encoding: String): RIO[Vector[String]] =
    readWithEncoding(encoding).map(_.lines.toVector)

  def readBytes: RIO[Array[Byte]] =
    RIO.using(this.toInputStream)(Streams.readBytes(_))

  def move(destination: LocalPath): RIO[Unit] =
    destination.unlessExists(s"File exists in target location $destination. Can not move source file $path",
      RIO.safe {
        val destFile = destination.toFile
        path.dirname.toFile.mkdirs
        this.toFile.renameTo(destFile)
      })

  def moveTo(destination: LocalDirectory): RIO[Unit] =
    path.basename match {
      case None =>
        RIO.fail("Source is a top level directory, can't move.")
      case Some(filename) =>
        move(destination.toLocalPath | filename)
    }

  def copy(destination: LocalPath): RIO[Unit] =
    destination.unlessExists(s"File exists in target location $destination. Can not move source file $path` ",
        destination.dirname.mkdirs >>
          RIO.using(RIO.safe[InputStream](new FileInputStream(toFile)))(destination.writeStream(_)))


  def copyTo(destination: LocalDirectory): RIO[Unit] =
    path.basename match {
      case None =>
        RIO.fail("Source is a top level directory, can't copy.")
      case Some(filename) =>
        val destLocalFile = destination.toLocalPath | filename
        RIO.using(RIO.safe[InputStream](new FileInputStream(toFile)))(destLocalFile.writeStream(_))
    }

}

object LocalFile {
  /** Construct a path from a java.io.File. */
  def fromFile(f: File): LocalFile =
    Option(f.getParentFile) match {
      case None =>
        val base = if (f.isAbsolute) new LocalFile(Root) else new LocalFile(Relative)
        if (f.getName.isEmpty) base else new LocalFile(Components(base.path, Component.unsafe(f.getName)))
      case Some(p) =>
        new LocalFile(fromFile(p).path </ Component.unsafe(f.getName))
    }

  def fromString(s: String): Option[LocalFile] =
    s.split("/").toList match {
      case "" :: Nil =>
        None
      case "" :: parts =>
        parts.traverse(Component.create).map(fromList(Root, _))
      case parts =>
        parts.traverse(Component.create).map(fromList(Relative, _))
    }

  def fromList(dir: Path, parts: List[Component]): LocalFile =
    new LocalFile(parts.foldLeft(dir)((acc, el) => acc </ el))

  def fromURI(s: URI): Option[LocalFile] =
    fromString(s.getPath)

  def unsafe(s: String): LocalFile =
    fromString(s).getOrElse(sys.error("LocalFile.unsafe on an invalid string."))

  implicit def LocalFileOrder: Order[LocalFile] =
    Order.order((x, y) => x.path.?|?(y.path))

  implicit def LocalFileOrdering =
    LocalFileOrder.toScalaOrdering
}
