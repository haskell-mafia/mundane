package com.ambiata.mundane.io

import com.ambiata.mundane.control._
import com.ambiata.mundane.data._
import com.ambiata.mundane.path._
import com.ambiata.mundane.io.LocalFile._
import java.io._
import java.net.URI
import java.util.UUID
import scala.io.Codec
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

  def optionExists[A](thunk: => RIO[A]): RIO[Option[A]] =
    exists >>= (e =>
      if   (e) thunk.map(_.some)
      else none.pure[RIO]
    )

  def unlessExists[A](error: String, thunk: => RIO[A]): RIO[A] =
    exists >>= (e =>
      if (e) RIO.failIO(error)
      else thunk
    )

  def delete: RIO[Unit] = for {
    e <- exists
  } yield !e || toFile.delete

  def read: RIO[Option[String]] =
    readWithEncoding(Codec.UTF8)

  def readOrFail: RIO[String] =
    read.flatMap(RIO.fromOption(_, "Failed to read file - file does not exist"))

  def readWithEncoding(encoding: Codec): RIO[Option[String]] =
    optionExists(RIO.using(this.toInputStream) { in => Streams.readWithEncoding(in, encoding) })

  def readLines: RIO[Option[List[String]]] =
    readLinesWithEncoding(Codec.UTF8)

  def readLinesWithEncoding(encoding: Codec): RIO[Option[List[String]]] =
    optionExists(readPerLineWithEncoding(encoding, scala.collection.mutable.ListBuffer[String]())((s, b) => { b += s; b }).map(_.toList))

  def readUnsafe(f: java.io.InputStream => RIO[Unit]): RIO[Unit] =
    RIO.using(this.toInputStream)(f)

  def doPerLine[A](f: String => RIO[Unit]): RIO[Unit] =
    RIO.using(this.toInputStream)(in =>
      RIO.io {
        val reader = new java.io.BufferedReader(new java.io.InputStreamReader(in, "UTF-8"))
        var line: String = null
        var result: RIO[Unit] = null
        while ({ line = reader.readLine; line != null && result == null })
          f(line).unsafePerformIO match {
            case Ok(_) =>
              ()
            case e @ Error(_) =>
              result = RIO.result[Unit](e)
          }
        }
      )

  def readPerLine[A](empty: => A)(f: (String, A) => A): RIO[A] =
    readPerLineWithEncoding(Codec.UTF8, empty)(f)

  def readPerLineWithEncoding[A](codec: Codec, empty: => A)(f: (String, A) => A): RIO[A] =
    RIO.io(empty).flatMap { s =>
      var state = s
      readUnsafe { in => RIO.io {
        val reader = new java.io.BufferedReader(new java.io.InputStreamReader(in, codec.name))
        var line: String = null
        while ({ line = reader.readLine; line != null })
          state = f(line, state)
      }}.as(state)
    }

  def readBytes: RIO[Option[Array[Byte]]] =
    optionExists(RIO.using(this.toInputStream)(Streams.readBytes(_)))

  def writeStream(content: InputStream): RIO[Unit] =
    RIO.using(path.toOutputStream)(Streams.pipe(content, _))

  def writeWithMode(content: String): RIO[Unit] = ???
  def writeWithEncodingMode(content: String): RIO[Unit] = ???
  def writeLinesWithMode(content: String): RIO[Unit] = ???
  def writeLinesWithEncodingMode(content: String): RIO[Unit] = ???
  def writeBytesWithMode(content: Array[Byte]): RIO[Unit] = ???

  def append(content: String): RIO[Unit] =
    appendWithEncoding(content, Codec.UTF8)

  def appendWithEncoding(content: String, encoding: Codec): RIO[Unit] =
    RIO.using(path.toOutputStream)(out => Streams.writeWithEncoding(out, content, encoding))

  def appendLines(content: List[String]): RIO[Unit] =
    appendLinesWithEncoding(content, Codec.UTF8)

  def appendLinesWithEncoding(content: List[String], encoding: Codec): RIO[Unit] =
    appendWithEncoding(Lists.prepareForFile(content), encoding)

  def appendBytes(content: Array[Byte]): RIO[Unit] =
    RIO.using(path.toOutputStream)(Streams.writeBytes(_, content))

  def overwrite(content: String): RIO[Unit] =
    overwriteWithEncoding(content, Codec.UTF8)

  def overwriteWithEncoding(content: String, encoding: Codec): RIO[Unit] =
    RIO.using(path.toOverwriteOutputStream)(out => Streams.writeWithEncoding(out, content, encoding))

  def overwriteLines(content: List[String]): RIO[Unit] =
    overwriteLinesWithEncoding(content, Codec.UTF8)

  def overwriteLinesWithEncoding(content: List[String], encoding: Codec): RIO[Unit] =
    overwriteWithEncoding(Lists.prepareForFile(content), encoding)

  def overwriteBytes(content: Array[Byte]): RIO[Unit] =
    RIO.using(path.toOverwriteOutputStream)(Streams.writeBytes(_, content))

/**
TODO
- write with mode ( overwrite or append or fail)
- move with mode ( overwrite or fail)
- move needs to assert source file exists
- add optionExists to LocalPath

  */

  def move(destination: LocalPath): RIO[LocalFile] =
    destination.unlessExists(s"File exists in target location $destination. Can not move source file $path",
      RIO.safe {
        val destFile = destination.toFile
        destination.dirname.toFile.mkdirs
        this.toFile.renameTo(destFile)
      }.as(LocalFile.unsafe(destination.path.path)))

  def moveWithMode(destination: LocalPath): RIO[LocalFile] = ???

  def moveTo(destination: LocalDirectory): RIO[Unit] =
    path.basename match {
      case None =>
        RIO.fail("Source is a top level directory, can't move.")
      case Some(filename) =>
        move(destination.toLocalPath | filename).void
    }

  def copy(destination: LocalPath): RIO[LocalFile] =
    destination.unlessExists(s"File exists in target location $destination. Can not move source file $path` ",
        destination.dirname.mkdirs >>
          RIO.using(RIO.safe[InputStream](new FileInputStream(toFile)))(destination.writeStream(_))).as(LocalFile.unsafe(destination.path.path))

  def copyWithMode(destination: LocalPath): RIO[LocalFile] = ???

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
        new LocalFile(fromFile(p).path | Component.unsafe(f.getName))
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
    new LocalFile(parts.foldLeft(dir)((acc, el) => acc | el))

  def fromURI(s: URI): Option[LocalFile] =
    fromString(s.getPath)

  def unsafe(s: String): LocalFile =
    fromString(s).getOrElse(sys.error("LocalFile.unsafe on an invalid string."))

  implicit def LocalFileOrder: Order[LocalFile] =
    Order.order((x, y) => x.path.?|?(y.path))

  implicit def LocalFileOrdering =
    LocalFileOrder.toScalaOrdering
}
