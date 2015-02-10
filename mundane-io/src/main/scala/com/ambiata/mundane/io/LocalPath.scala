package com.ambiata.mundane.io

import com.ambiata.mundane.control._
import com.ambiata.mundane.data._
import com.ambiata.mundane.path._
import java.io._
import java.net.URI
import java.util.Date
import scala.io.Codec
import scalaz._, Scalaz._, effect._, Effect._
import MemoryConversions._

/**
 * 'LocalPath' is an unknown local location which means that
 * either nothing exists at that location or that possibly
 * something exists and we just don't know yet. A file that
 * is known to exist is denoted by either 'LocalFile' or
 * 'LocalDirectory'.
 */
case class LocalPath(path: Path) {
  def /(other: Path): LocalPath =
    LocalPath(path / other)

  def join(other: Path): LocalPath =
    /(other)

  def |(other: Component): LocalPath =
    LocalPath(path | other)

  def extend(other: Component): LocalPath =
    |(other)

  def /-(other: String): LocalPath =
    LocalPath(path /- other)

  def rebaseTo(other: LocalPath): Option[LocalPath] =
    path.rebaseTo(other.path).map(LocalPath(_))

  def toFile: File =
    path.toFile

  def dirname: LocalPath =
    LocalPath(path.dirname)

  def basename: Option[Component] =
    path.basename

  def exists: RIO[Boolean] =
    determine.map(_.isDefined)

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

  def delete: RIO[Unit] =
    determinef(_.delete, _.delete)

  def determine: RIO[Option[LocalFile \/ LocalDirectory]] = {
    val file = path.toFile
    for {
      e <- RIO.safe[Boolean](file.exists)
      o <- if (e)
        RIO.safe[Boolean](file.isFile) >>= ((f: Boolean) => (
          if (f) LocalFile.unsafe(path.path).left
          else   LocalDirectory.unsafe(path.path).right
        ).some.pure[RIO])
      else
        none.pure[RIO]
    } yield o
  }

  def determinef[A](file: LocalFile => RIO[A], directory: LocalDirectory => RIO[A]): RIO[A] =
    determinefWith(file, directory, RIO.failIO(s"Not a valid File or Directory. LocalPath($path)"))

  def determinefWithPure[A](file: LocalFile => A, directory: LocalDirectory => A, none: A): RIO[A] =
    determine >>= ({
      case Some(\/-(v)) =>
        directory(v).pure[RIO]
      case Some(-\/(v)) =>
        file(v).pure[RIO]
      case None =>
        none.pure[RIO]
    })

  def determinefWith[A](file: LocalFile => RIO[A], directory: LocalDirectory => RIO[A], none: RIO[A]): RIO[A] =
    determine >>= ({
      case Some(\/-(v)) =>
        directory(v)
      case Some(-\/(v)) =>
        file(v)
      case None =>
        none
    })

  def determineFile: RIO[LocalFile] =
    determinef(_.pure[RIO], _ => RIO.fail(s"Not a valid file. LocalDirectory($path)"))

  def determineDirectory: RIO[LocalDirectory] =
    determinef(_ => RIO.fail(s"Not a valid directory. LocalFile($path)"), _.pure[RIO])

  def readWith[A](thunk: LocalFile => RIO[A]): RIO[A] =
    determinefWith(
        thunk
      , _ => RIO.fail(s"Can not read a directory, LocalDirectory($path).")
      , RIO.fail(s"Can not read nothing, $path."))

  def checksum(algorithm: ChecksumAlgorithm): RIO[Option[Checksum]] =
    readWithOption(_.checksum(algorithm))

  def lineCount: RIO[Option[Int]] =
    readWithOption(_.lineCount)

  def readWithOption[A](thunk: LocalFile => RIO[Option[A]]): RIO[Option[A]] =
    determinefWith(
        thunk
      , _ => RIO.fail(s"Can not read a directory, LocalDirectory($path).")
      , none.pure[RIO])

  def read: RIO[Option[String]] =
    readWithOption(_.read)

  def readOrFail: RIO[String] =
    readWith(_.readOrFail)

  def readWithEncoding(encoding: Codec): RIO[Option[String]] =
    readWithOption(_.readWithEncoding(encoding))

  def readLines: RIO[Option[List[String]]] =
    readWithOption(_.readLines)

  def readLinesWithEncoding(encoding: Codec): RIO[Option[List[String]]] =
    readWithOption(_.readLinesWithEncoding(encoding))

  def readUnsafe(f: java.io.InputStream => RIO[Unit]): RIO[Unit] =
    readWith(_.readUnsafe(f))

  def doPerLine[A](f: String => RIO[Unit]): RIO[Unit] =
    readWith(_.doPerLine(f))

  def readPerLine[A](empty: => A)(f: (String, A) => A): RIO[A] =
    readWith(_.readPerLine(empty)(f))

  def readPerLineWithEncoding[A](codec: Codec, empty: => A)(f: (String, A) => A): RIO[A] =
    readWith(_.readPerLineWithEncoding(codec, empty)(f))

  def readBytes: RIO[Option[Array[Byte]]] =
    readWithOption(_.readBytes)

  def touch: RIO[LocalFile] = {
    for {
      e <- exists
      r <- if (e) RIO.safe[Unit]({ toFile.setLastModified(System.currentTimeMillis); () }).as(LocalFile.unsafe(path.path))
           else   write("")
    } yield r
  }

  def append(content: String): RIO[LocalFile] =
    appendWithEncoding(content, Codec.UTF8)

  def appendWithEncoding(content: String, encoding: Codec): RIO[LocalFile] =
    determinefWith(
        _.appendWithEncoding(content, encoding).as(LocalFile.unsafe(path.path))
      , _ => RIO.fail(s"Can not append to a directory, LocalDirectory($path)")
      , writeWithEncoding(content, encoding))

  def appendLines(content: List[String]): RIO[LocalFile] =
    appendLinesWithEncoding(content, Codec.UTF8)

  def appendLinesWithEncoding(content: List[String], encoding: Codec): RIO[LocalFile] =
    appendWithEncoding(Lists.prepareForFile(content), encoding)

  def appendBytes(content: Array[Byte]): RIO[LocalFile] =
    RIO.using(path.toOutputStream)(Streams.writeBytes(_, content)).as(LocalFile.unsafe(path.path))

  def writeExists[A](thunk: => RIO[A]): RIO[A] =
    doesNotExist(s"A file or directory already exists in the specified location, LocalPath($path).", thunk)

  def write(content: String): RIO[LocalFile] =
    writeWithEncoding(content, Codec.UTF8)

  def writeWithEncoding(content: String, encoding: Codec): RIO[LocalFile] = writeExists(for {
    _ <- dirname.mkdirs
    _ <- RIO.using(path.toOutputStream) { out => Streams.writeWithEncoding(out, content, encoding) }
  } yield LocalFile.unsafe(path.path))

  def writeLines(content: List[String]): RIO[LocalFile] =
    writeLinesWithEncoding(content, Codec.UTF8)

  def writeLinesWithEncoding(content: List[String], encoding: Codec): RIO[LocalFile] = writeExists(for {
    _ <- dirname.mkdirs
    _ <- writeWithEncoding(Lists.prepareForFile(content), encoding)
  } yield LocalFile.unsafe(path.path))

  def writeBytes(content: Array[Byte]): RIO[LocalFile] = writeExists(for {
    _ <- dirname.mkdirs
    _ <- RIO.using(path.toOutputStream)(Streams.writeBytes(_, content))
  } yield LocalFile.unsafe(path.path))

  def writeStream(content: InputStream): RIO[LocalFile] = writeExists(for {
    _ <- dirname.mkdirs
    _ <- RIO.using(path.toOutputStream)(Streams.pipe(content, _))
  } yield LocalFile.unsafe(path.path))

  def writeWithMode(content: String, mode: WriteMode): RIO[LocalFile] =
    mode.fold(append(content), overwrite(content), write(content))

  def writeWithEncodingMode(content: String, encoding: Codec, mode: WriteMode): RIO[LocalFile] =
    mode.fold(
        appendWithEncoding(content, encoding)
      , overwriteWithEncoding(content, encoding)
      , writeWithEncoding(content, encoding))

  def writeLinesWithMode(content: List[String], mode: WriteMode): RIO[LocalFile] =
    mode.fold(appendLines(content), overwriteLines(content), writeLines(content))

  def writeLinesWithEncodingMode(content: List[String], encoding: Codec, mode: WriteMode): RIO[LocalFile] =
    mode.fold(
        appendLinesWithEncoding(content, encoding)
      , overwriteLinesWithEncoding(content, encoding)
      , writeLinesWithEncoding(content, encoding))

  def writeBytesWithMode(content: Array[Byte], mode: WriteMode): RIO[LocalFile] =
    mode.fold(appendBytes(content), overwriteBytes(content), writeBytes(content))

  def overwriteStream(content: InputStream): RIO[LocalFile] =
    RIO.using(path.toOverwriteOutputStream)(Streams.pipe(content, _)).as(LocalFile.unsafe(path.path))

  def overwrite(content: String): RIO[LocalFile] =
    overwriteWithEncoding(content, Codec.UTF8)

  def overwriteWithEncoding(content: String, encoding: Codec): RIO[LocalFile] =
    determinefWith(
        _.overwriteWithEncoding(content, encoding).as(LocalFile.unsafe(path.path))
      , _ => RIO.fail(s"Can not overwrite a directory, LocalDirectory($path)")
      , writeWithEncoding(content, encoding))

  def overwriteLines(content: List[String]): RIO[LocalFile] =
    overwriteLinesWithEncoding(content, Codec.UTF8)

  def overwriteLinesWithEncoding(content: List[String], encoding: Codec): RIO[LocalFile] =
    overwriteWithEncoding(Lists.prepareForFile(content), encoding)

  def overwriteBytes(content: Array[Byte]): RIO[LocalFile] =
    RIO.using(path.toOverwriteOutputStream)(Streams.writeBytes(_, content)).as(LocalFile.unsafe(path.path))

  def mkdirs: RIO[LocalDirectory] =
    RIO.safe[Boolean](path.toFile.mkdirs).void >>
      RIO.ok(LocalDirectory.unsafe(path.path))

   /** List all files, will not include directories */
  def listFilesRecursively: RIO[List[LocalFile]] =
    determinef(v => List(v).pure[RIO], d => d.listFilesRecursively)

  def listDirectoriesRecursively: RIO[List[LocalDirectory]] =
    determinef(_ => nil.pure[RIO], d => d.listDirectoriesRecursively)

  /** This will list all Path's including directories  */
  def listPathsRecursively: RIO[List[LocalPath]] =
    determinef(v => List(LocalPath(path)).pure[RIO], d => d.listPathsRecursively)

  /** This will only list directories for a single level */
  def listDirectories: RIO[List[LocalDirectory]] =
    determinef(_ => nil.pure[RIO], d => d.listDirectories)

  /** This will only list files for a single level */
  def listFiles: RIO[List[LocalFile]] =
    determinef(_ => List(LocalPath(path)).traverseU(_.determineFile), d => d.listFiles)

  /** This will only list Path's for a single level (LocalFile and LocalDirectory) */
  def listPaths: RIO[List[LocalPath]] =
    determinef(v => List(LocalPath(path)).pure[RIO], d => d.listPaths)

  def size: RIO[BytesQuantity] =
    listFilesRecursively.map(_.foldMap(_.toFile.length).bytes)

  def move(destination: LocalPath): RIO[Unit] =
    determinef(file =>
      destination.determinefWith(
          _ => RIO.failIO(s"File exists in target location $destination. Can not move $path file.")
        , d => file.moveTo(d).void
        , file.move(destination).void)
      , dir =>
      destination.determinefWith(
          _ => RIO.failIO(s"File eixsts in the target location $destination. Can not move $path directory.")
        , d => dir.moveTo(d).void
        , dir.move(destination).void
      ))

  def copy(destination: LocalPath): RIO[Unit] =
    determinef(file =>
      destination.determinefWith(
          _ => RIO.failIO(s"File exists in target location $destination. Can not move $path file.")
        , d => file.copyTo(d).void
        , file.copy(destination).void)
      , dir =>
        RIO.fail(s"Copying from a LocalDirectory(path) is current an unsupported operation"))

}

object LocalPath {
  implicit def LocalPathOrder: Order[LocalPath] =
    Order.order((x, y) => x.path.?|?(y.path))

  implicit def LocalPathOrdering: scala.Ordering[LocalPath] =
    LocalPathOrder.toScalaOrdering

  def fromList(dir: Path, components: List[Component]): LocalPath =
    LocalPath(Path.fromList(dir, components))

  def fromString(s: String): LocalPath =
    LocalPath(Path(s))

  def fromFile(f: File): RIO[LocalPath] = for {
    o <- RIO.io(Option(f.getParentFile))
    r <- o match {
      case None =>
        RIO.safe({
          val base = if (f.isAbsolute) new LocalPath(Root) else new LocalPath(Relative)
          if (f.getName.isEmpty) base else new LocalPath(Components(base.path, Component.unsafe(f.getName)))
        })
      case Some(p) =>
        fromFile(p).map(p => new LocalPath(p.path | Component.unsafe(f.getName)))
    }
  } yield r

  def fromURI(s: URI): Option[LocalPath] =
    s.getScheme match {
      case "file" =>
        fromString(s.getPath).some
      case null =>
        fromString(s.getPath).some
      case _ =>
        none
    }
}
