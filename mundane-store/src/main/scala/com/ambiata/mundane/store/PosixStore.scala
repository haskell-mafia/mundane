package com.ambiata.mundane.store

import com.ambiata.mundane.control._
import com.ambiata.mundane.io._
import java.io.{InputStream, OutputStream}
import scala.io.Codec
import scalaz._, Scalaz._, scalaz.stream._, scalaz.concurrent._, effect.IO
import scodec.bits.ByteVector

case class PosixStore(root: FilePath) extends Store[ResultTIO] {
  def normalize(file: FilePath): String =
    file.absolute.path.replace(root.absolute.path + "/", "")

  def list(prefix: FilePath): ResultT[IO, List[FilePath]] =
    Directories.list(root </> prefix).map(_.map(normalize).sorted.map(_.toFilePath))

  def filter(prefix: FilePath, predicate: FilePath => Boolean): ResultT[IO, List[FilePath]] =
    list(prefix).map(_.filter(predicate))

  def find(prefix: FilePath, predicate: FilePath => Boolean): ResultT[IO, Option[FilePath]] =
    list(prefix).map(_.find(predicate))

  def exists(path: FilePath): ResultT[IO, Boolean] =
    Files.exists(root </> path)

  def delete(path: FilePath): ResultT[IO, Unit] =
    Files.delete(root </> path)

  def deleteAll(prefix: FilePath): ResultT[IO, Unit] =
    Directories.delete(root </> prefix)

  def move(in: FilePath, out: FilePath): ResultT[IO, Unit] =
    ???

  def copy(in: FilePath, out: FilePath): ResultT[IO, Unit] =
    ???

  def mirror(in: FilePath, out: FilePath): ResultT[IO, Unit] =
    ???

  def moveTo(store: Store[ResultTIO], in: FilePath, out: FilePath): ResultT[IO, Unit] =
    ???

  def copyTo(store: Store[ResultTIO], in: FilePath, out: FilePath): ResultT[IO, Unit] =
    ???

  def mirrorTo(store: Store[ResultTIO], in: FilePath, out: FilePath): ResultT[IO, Unit] =
    ???

  def checksum(path: FilePath, algorithm: ChecksumAlgorithm): ResultT[IO, Checksum] =
    ???

  val bytes: StoreBytes[ResultTIO] = new StoreBytes[ResultTIO] {
    def read(path: FilePath): ResultT[IO, ByteVector] =
      ???

    def write(path: FilePath, data: ByteVector): ResultT[IO, Unit] =
      ???

    def source(path: FilePath): Process[Task, ByteVector] =
      ???

    def sink(path: FilePath): Sink[Task, ByteVector] =
      ???
  }

  val strings: StoreStrings[ResultTIO] = new StoreStrings[ResultTIO] {
    def read(path: FilePath, codec: Codec): ResultT[IO, String] =
      ???

    def write(path: FilePath, data: String, codec: Codec): ResultT[IO, Unit] =
      ???

    def source(path: FilePath, codec: Codec): Process[Task, String] =
      ???

    def sink(path: FilePath, codec: Codec): Sink[Task, String] =
      ???
  }

  val utf8: StoreUtf8[ResultTIO] = new StoreUtf8[ResultTIO] {
    def read(path: FilePath): ResultT[IO, String] =
      ???

    def write(path: FilePath, data: String): ResultT[IO, Unit] =
      ???

    def source(path: FilePath): Process[Task, String] =
      ???

    def sink(path: FilePath): Sink[Task, String] =
      ???
  }

  val lines: StoreLines[ResultTIO] = new StoreLines[ResultTIO] {
    def read(path: FilePath, codec: Codec): ResultT[IO, List[String]] =
      ???

    def write(path: FilePath, data: List[String], codec: Codec): ResultT[IO, Unit] =
      ???

    def source(path: FilePath, codec: Codec): Process[Task, String] =
      ???

    def sink(path: FilePath, codec: Codec): Sink[Task, String] =
      ???
  }

  val linesUtf8: StoreLinesUtf8[ResultTIO] = new StoreLinesUtf8[ResultTIO] {
    def read(path: FilePath): ResultT[IO, List[String]] =
      ???

    def write(path: FilePath, data: List[String]): ResultT[IO, Unit] =
      ???

    def source(path: FilePath): Process[Task, String] =
      ???

    def sink(path: FilePath): Sink[Task, String] =
      ???
  }

  val unsafe: StoreUnsafe[ResultTIO] = new StoreUnsafe[ResultTIO] {
    def withInputStream(path: FilePath)(f: InputStream => ResultT[IO, Unit]): ResultT[IO, Unit] =
      ???

    def withOutputStream(path: FilePath)(f: OutputStream => ResultT[IO, Unit]): ResultT[IO, Unit] =
      ???
  }
}
