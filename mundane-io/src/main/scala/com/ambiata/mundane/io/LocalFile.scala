package com.ambiata.mundane.io

import com.ambiata.mundane.control._
import com.ambiata.mundane.data._
import com.ambiata.mundane.path._
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

  def toInputStream: RIO[InputStream] =
    RIO.safe(new FileInputStream(path.path))

// ============= Operations =================

  def exists: RIO[Boolean] = for {
    f <- RIO.safe(toFile.isFile)
    d <- RIO.safe(toFile.isDirectory)
    r <- if (f)      RIO.ok(true)
         else if (d) RIO.fail(s"An internal invariant has been violated, the $path points to a directory.")
         else        RIO.ok(false)
  } yield r

  def onExists[A](success: => RIO[A], missing: => RIO[A]): RIO[A] =
    exists >>= (e =>
      if (e) success
      else   missing
    )

  def optionExists[A](thunk: => RIO[A]): RIO[Option[A]] =
    onExists(thunk.map(_.some), none.pure[RIO])

  def whenExists(thunk: => RIO[Unit]): RIO[Unit] =
    onExists(thunk, RIO.unit)

  def doesExist[A](error: String, thunk: => RIO[A]): RIO[A] =
    onExists(thunk, RIO.failIO(error))

  def doesNotExist[A](error: String, thunk: => RIO[A]): RIO[A] =
    onExists(RIO.failIO(error), thunk)

  def delete: RIO[Unit] =
    whenExists(RIO.safe(toFile.delete).void)

  def read: RIO[Option[String]] =
    readWithEncoding(Codec.UTF8)

  def readOrFail: RIO[String] =
    read.flatMap(RIO.fromOption(_, s"Failed to read file - LocalFile($path) does not exist"))

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

  def checksum(algorithm: ChecksumAlgorithm): RIO[Option[Checksum]] =
    optionExists(RIO.using(toInputStream)(in => Checksum.stream(in, algorithm)))

  def lineCount: RIO[Option[Int]] =
    optionExists(RIO.io({
      val reader = new LineNumberReader(new FileReader(toFile))
      try {
        while (reader.readLine != null) {}
          reader.getLineNumber
      } finally reader.close
    }))

  def writeStream(content: InputStream): RIO[Unit] = for {
    _ <- toLocalPath.dirname.mkdirs
    _ <- RIO.using(path.toOutputStream)(Streams.pipe(content, _))
  } yield ()

  def writeWithMode(content: String, mode: WriteMode): RIO[Unit] =
    mode.fold(append(content), overwrite(content), toLocalPath.write(content)).void

  def writeWithEncodingMode(content: String, encoding: Codec, mode: WriteMode): RIO[Unit] =
    mode.fold(
        appendWithEncoding(content, encoding)
      , overwriteWithEncoding(content, encoding)
      , toLocalPath.writeWithEncoding(content, encoding)).void

  def writeLinesWithMode(content: List[String], mode: WriteMode
): RIO[Unit] =
    mode.fold(appendLines(content), overwriteLines(content), toLocalPath.writeLines(content)).void

  def writeLinesWithEncodingMode(content: List[String], encoding: Codec, mode: WriteMode): RIO[Unit] =
    mode.fold(
        appendLinesWithEncoding(content, encoding)
      , overwriteLinesWithEncoding(content, encoding)
      , toLocalPath.writeLinesWithEncoding(content, encoding)).void

  def writeBytesWithMode(content: Array[Byte], mode: WriteMode): RIO[Unit] =
    mode.fold(appendBytes(content), overwriteBytes(content), toLocalPath.writeBytes(content)).void

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

  def move(destination: LocalPath): RIO[LocalFile] =
    doesExist(s"Source file does not exist. LocalFile($path)",
      destination.doesNotExist(s"A file/directory exists in target location $destination. Can not move source file LocalFile($path)",
        RIO.safe {
          val destFile = destination.toFile
          destination.dirname.toFile.mkdirs
          this.toFile.renameTo(destFile)
        }.as(LocalFile.unsafe(destination.path.path))))

  def moveWithMode(destination: LocalPath, mode: TargetMode): RIO[LocalFile] =
    mode.fold(doesExist(s"Source file does not exist. LocalFile($path)",
      RIO.safe {
        val destFile = destination.toFile
        destination.dirname.toFile.mkdirs
        this.toFile.renameTo(destFile)
      }.as(LocalFile.unsafe(destination.path.path))), move(destination))

  def moveTo(destination: LocalDirectory): RIO[LocalFile] =
    (path.basename match {
      case None =>
        RIO.fail(s"Source is a top level directory, can't move. Source($path)")
      case Some(filename) =>
        move(destination.toLocalPath | filename)
    })

  def copy(destination: LocalPath): RIO[LocalFile] =
    doesExist(s"Source file does not exist. LocalFile($path)",
      destination.doesNotExist(s"A file/directory exists in target location $destination. Can not move source file LocalFile($path)",
        destination.dirname.mkdirs >>
          RIO.using(RIO.safe[InputStream](new FileInputStream(toFile)))(destination.writeStream(_))).as(LocalFile.unsafe(destination.path.path)))

  def copyWithMode(destination: LocalPath, mode: TargetMode): RIO[LocalFile] =
    mode.fold(doesExist(s"Source file does not exist. LocalFile($path)",
      destination.dirname.mkdirs >>
        RIO.using(RIO.safe[InputStream](new FileInputStream(toFile)))(destination.overwriteStream(_))).as(LocalFile.unsafe(destination.path.path)),
      copy(destination))

  def copyTo(destination: LocalDirectory): RIO[LocalFile] =
    path.basename match {
      case None =>
        RIO.fail(s"Source is a top level directory, can't copy. Source($path)")
      case Some(filename) =>
        val destLocalFile = destination.toLocalPath | filename
        RIO.using(RIO.safe[InputStream](new FileInputStream(toFile)))(destLocalFile.writeStream(_)).as(
          LocalFile.unsafe(destLocalFile.path.path))
    }
}

object LocalFile {
  /** Construct a path from a java.io.File. */
  def fromFile(f: File): RIO[LocalFile] = for {
    o <- RIO.io(Option(f.getParentFile))
    r <- o match {
      case None =>
        RIO.safe({
          val base = if (f.isAbsolute) new LocalFile(Root) else new LocalFile(Relative)
          if (f.getName.isEmpty) base else new LocalFile(Components(base.path, Component.unsafe(f.getName)))
        })
      case Some(p) =>
        fromFile(p).map(p => new LocalFile(p.path | Component.unsafe(f.getName)))
    }
  } yield r

  def fromString(s: String): RIO[Option[LocalFile]] =
    s.split("/").toList match {
      case "" :: Nil =>
        none.pure[RIO]
      case "" :: parts =>
        parts.traverse(Component.create).map(fromList(Root, _)).sequence.map(_.flatten)
      case parts =>
        parts.traverse(Component.create).map(fromList(Relative, _)).sequence.map(_.flatten)
    }

  def fromList(dir: Path, parts: List[Component]): RIO[Option[LocalFile]] = {
    val f = fromListP(dir, parts)
    f.optionExists(f.pure[RIO])
  }

  private def fromStringP(s: String): Option[LocalFile] =
    s.split("/").toList match {
      case "" :: Nil =>
        none
      case "" :: parts =>
        parts.traverse(Component.create).map(fromListP(Root, _))
      case parts =>
        parts.traverse(Component.create).map(fromListP(Relative, _))
    }

  private def fromListP(dir: Path, parts: List[Component]): LocalFile =
    new LocalFile(parts.foldLeft(dir)((acc, el) => acc | el))

  def fromURI(s: URI): RIO[Option[LocalFile]] =
    s.getScheme match {
      case "file" =>
        fromString(s.getPath)
      case null =>
        fromString(s.getPath)
      case _ =>
        none.pure[RIO]
    }

  private[io] def unsafe(s: String): LocalFile =
    fromStringP(s).getOrElse(sys.error(s"LocalFile.unsafe on an invalid string. String($s)"))

  def filterHidden(l: List[LocalFile]): List[LocalFile] =
    l.filter(f => !List(".", "_").exists(c => f.toPath.basename.exists(_.name.startsWith(c))))

  implicit def LocalFileOrder: Order[LocalFile] =
    Order.order((x, y) => x.path.?|?(y.path))

  implicit def LocalFileOrdering: scala.Ordering[LocalFile] =
    LocalFileOrder.toScalaOrdering
}
