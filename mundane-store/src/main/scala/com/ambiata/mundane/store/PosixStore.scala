package com.ambiata.mundane.store

import com.ambiata.mundane.control._
import com.ambiata.mundane.io._
import com.ambiata.mundane.data._
import java.io.{InputStream, OutputStream}
import scala.io.Codec
import scalaz._, Scalaz._, scalaz.stream._, scalaz.concurrent._, effect.IO, effect.Effect._
import scodec.bits.ByteVector

// FIX pull out "derived" functions so the implementation can be shared with s3/hdfs impls.
case class PosixStore(root: DirPath) extends Store[ResultTIO] with ReadOnlyStore[ResultTIO] {
  def readOnly: ReadOnlyStore[ResultTIO] =
    this

  def list(prefix: DirPath): ResultT[IO, List[FilePath]] =
    Directories.list(root </> prefix).map(_.map(_.relativeTo(root)))

  def filter(prefix: DirPath, predicate: FilePath => Boolean): ResultT[IO, List[FilePath]] =
    list(prefix).map(_.filter(predicate))

  def find(prefix: DirPath, predicate: FilePath => Boolean): ResultT[IO, Option[FilePath]] =
    list(prefix).map(_.find(predicate))

  def exists(path: FilePath): ResultT[IO, Boolean] =
    Files.exists(root </> path)

  def exists(path: DirPath): ResultT[IO, Boolean] =
    Directories.exists(root </> path)

  def delete(path: FilePath): ResultT[IO, Unit] =
    Files.delete(root </> path)

  def deleteAll(prefix: DirPath): ResultT[IO, Unit] =
    Directories.delete(root </> prefix).void

  def move(in: FilePath, out: FilePath): ResultT[IO, Unit] =
    Files.move(root </> in, root </> out)

  def copy(in: FilePath, out: FilePath): ResultT[IO, Unit] =
    Files.copy(root </> in, root </> out)

  def mirror(in: DirPath, out: DirPath): ResultT[IO, Unit] = for {
    paths <- list(in)
    _     <- paths.traverseU { source => copy(source, out </> source) }
  } yield ()

  def moveTo(store: Store[ResultTIO], src: FilePath, dest: FilePath): ResultT[IO, Unit] =
    copyTo(store, src, dest) >> delete(src)

  def copyTo(store: Store[ResultTIO], src: FilePath, dest: FilePath): ResultT[IO, Unit] =
    unsafe.withInputStream(src) { in =>
      store.unsafe.withOutputStream(dest) { out =>
        Streams.pipe(in, out) }}

  def mirrorTo(store: Store[ResultTIO], in: DirPath, out: DirPath): ResultT[IO, Unit] = for {
    paths <- list(in)
    _     <- paths.traverseU { source => copyTo(store, source, out </> source) }
  } yield ()

  def checksum(path: FilePath, algorithm: ChecksumAlgorithm): ResultT[IO, Checksum] =
    Checksum.file(root </> path, algorithm)

  val bytes: StoreBytes[ResultTIO] = new StoreBytes[ResultTIO] {
    def read(path: FilePath): ResultT[IO, ByteVector] =
      Files.readBytes(root </> path).map(ByteVector.apply)

    def write(path: FilePath, data: ByteVector): ResultT[IO, Unit] =
      Files.writeBytes(root </> path, data.toArray)

    def source(path: FilePath): Process[Task, ByteVector] =
      scalaz.stream.io.chunkR(new java.io.FileInputStream((root </> path).path)).evalMap(_(1024 * 1024))

    def sink(path: FilePath): Sink[Task, ByteVector] =
      scalaz.stream.io.chunkW(new java.io.FileOutputStream((root </> path).path))
  }

  val strings: StoreStrings[ResultTIO] = new StoreStrings[ResultTIO] {
    def read(path: FilePath, codec: Codec): ResultT[IO, String] =
      Files.read(root </> path, codec.name)

    def write(path: FilePath, data: String, codec: Codec): ResultT[IO, Unit] =
      Files.write(root </> path, data, codec.name)
  }

  val utf8: StoreUtf8[ResultTIO] = new StoreUtf8[ResultTIO] {
    def read(path: FilePath): ResultT[IO, String] =
      strings.read(path, Codec.UTF8)

    def write(path: FilePath, data: String): ResultT[IO, Unit] =
      strings.write(path, data, Codec.UTF8)

    def source(path: FilePath): Process[Task, String] =
      bytes.source(path) |> scalaz.stream.text.utf8Decode

    def sink(path: FilePath): Sink[Task, String] =
      bytes.sink(path).map(_.contramap(s => ByteVector.view(s.getBytes("UTF-8"))))
  }

  val lines: StoreLines[ResultTIO] = new StoreLines[ResultTIO] {
    def read(path: FilePath, codec: Codec): ResultT[IO, List[String]] =
      strings.read(path, codec).map(_.lines.toList)

    def write(path: FilePath, data: List[String], codec: Codec): ResultT[IO, Unit] =
      strings.write(path, Lists.prepareForFile(data), codec)

    def source(path: FilePath, codec: Codec): Process[Task, String] =
      scalaz.stream.io.linesR(new java.io.FileInputStream((root </> path).path))(codec)

    def sink(path: FilePath, codec: Codec): Sink[Task, String] =
      bytes.sink(path).map(_.contramap(s => ByteVector.view(s"$s\n".getBytes(codec.name))))
  }

  val linesUtf8: StoreLinesUtf8[ResultTIO] = new StoreLinesUtf8[ResultTIO] {
    def read(path: FilePath): ResultT[IO, List[String]] =
      lines.read(path, Codec.UTF8)

    def write(path: FilePath, data: List[String]): ResultT[IO, Unit] =
      lines.write(path, data, Codec.UTF8)

    def source(path: FilePath): Process[Task, String] =
      lines.source(path, Codec.UTF8)

    def sink(path: FilePath): Sink[Task, String] =
      lines.sink(path, Codec.UTF8)
  }

  val unsafe: StoreUnsafe[ResultTIO] = new StoreUnsafe[ResultTIO] {
    def withInputStream(path: FilePath)(f: InputStream => ResultT[IO, Unit]): ResultT[IO, Unit] =
      ResultT.using((root </> path).toInputStream)(f)

    def withOutputStream(path: FilePath)(f: OutputStream => ResultT[IO, Unit]): ResultT[IO, Unit] =
      Directories.mkdirs((root </> path).dirname) >> ResultT.using((root </> path).toOutputStream)(f)
  }
}
