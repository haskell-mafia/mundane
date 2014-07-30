package com.ambiata.mundane.store

import com.ambiata.mundane.control.{ResultT, ResultTIO}
import com.ambiata.mundane.io._
import java.io.{InputStream, OutputStream}
import scala.io.Codec
import scalaz._, Scalaz._, scalaz.stream._, scalaz.concurrent._
import scodec.bits.ByteVector

trait Store[F[_]] extends WriteOnlyStore[F] with ReadOnlyStore[F] {
  val bytes: StoreBytes[F]
  val strings: StoreStrings[F]
  val utf8: StoreUtf8[F]
  val lines: StoreLines[F]
  val linesUtf8: StoreLinesUtf8[F]
  val unsafe: StoreUnsafe[F]
}

trait WriteOnlyStore[F[_]] {
  def delete(path: FilePath): F[Unit]
  def delete(prefix: DirPath): F[Unit]
  def deleteAll: F[Unit] = delete(DirPath.Root)

  def move(in: FilePath, out: FilePath): F[Unit]
  def moveTo(store: Store[F], in: FilePath, out: FilePath): F[Unit]

  def copy(in: FilePath, out: FilePath): F[Unit]
  def mirror(out: DirPath): F[Unit] = mirror(DirPath.Root, out)
  def mirror(in: DirPath, out: DirPath): F[Unit]

  val bytes: StoreBytesWrite[F]
  val strings: StoreStringsWrite[F]
  val utf8: StoreUtf8Write[F]
  val lines: StoreLinesWrite[F]
  val linesUtf8: StoreLinesUtf8Write[F]
  val unsafe: StoreUnsafeWrite[F]
}

trait ReadOnlyStore[F[_]] {
  def listAll: F[List[FilePath]] = list(DirPath.Root)
  def list(prefix: DirPath): F[List[FilePath]]

  def filterAll(predicate: FilePath => Boolean): F[List[FilePath]] = filter(DirPath.Root, predicate)
  def filter(prefix: DirPath, predicate: FilePath => Boolean): F[List[FilePath]]

  def findAll(predicate: FilePath => Boolean): F[Option[FilePath]] = find(DirPath.Root, predicate)
  def find(prefix: DirPath, predicate: FilePath => Boolean): F[Option[FilePath]]

  def exists(path: FilePath): F[Boolean]
  def exists(path: DirPath): F[Boolean]

  def copyTo(store: Store[F], in: FilePath, out: FilePath): F[Unit]

  def mirrorTo(store: Store[F]): F[Unit] = mirrorTo(store, DirPath.Root)
  def mirrorTo(store: Store[F], out: DirPath): F[Unit] = mirrorTo(store, DirPath.Root, out)
  def mirrorTo(store: Store[F], in: DirPath, out: DirPath): F[Unit]

  def checksum(path: FilePath, algorithm: ChecksumAlgorithm): F[Checksum]

  val bytes: StoreBytesRead[F]
  val strings: StoreStringsRead[F]
  val utf8: StoreUtf8Read[F]
  val lines: StoreLinesRead[F]
  val linesUtf8: StoreLinesUtf8Read[F]
  val unsafe: StoreUnsafeRead[F]
}

trait StoreBytes[F[_]] extends StoreBytesRead[F] with StoreBytesWrite[F]

trait StoreBytesRead[F[_]] {
  def read(path: FilePath): F[ByteVector]
  def source(path: FilePath): Process[Task, ByteVector]
}

trait StoreBytesWrite[F[_]] {
  def write(path: FilePath, data: ByteVector): F[Unit]
  def sink(path: FilePath): Sink[Task, ByteVector]
}

trait StoreStrings[F[_]] extends StoreStringsRead[F] with StoreStringsWrite[F]

trait StoreStringsRead[F[_]] {
  def read(path: FilePath, codec: Codec): F[String]
}

trait StoreStringsWrite[F[_]] {
  def write(path: FilePath, data: String, codec: Codec): F[Unit]
}

trait StoreUtf8[F[_]] extends StoreUtf8Read[F] with StoreUtf8Write[F]

trait StoreUtf8Read[F[_]] {
  def read(path: FilePath): F[String]
  def source(path: FilePath): Process[Task, String]
}

trait StoreUtf8Write[F[_]] {
  def write(path: FilePath, data: String): F[Unit]
  def sink(path: FilePath): Sink[Task, String]
}

trait StoreLines[F[_]] extends StoreLinesRead[F] with StoreLinesWrite[F]

trait StoreLinesRead[F[_]] {
  def read(path: FilePath, codec: Codec): F[List[String]]
  def source(path: FilePath, codec: Codec): Process[Task, String]
}

trait StoreLinesWrite[F[_]] {
  def write(path: FilePath, data: List[String], codec: Codec): F[Unit]
  def sink(path: FilePath, codec: Codec): Sink[Task, String]
}

trait StoreLinesUtf8[F[_]] extends StoreLinesUtf8Read[F] with StoreLinesUtf8Write[F]

trait StoreLinesUtf8Read[F[_]]  {
  def read(path: FilePath): F[List[String]]
  def source(path: FilePath): Process[Task, String]
}

trait StoreLinesUtf8Write[F[_]]  {
  def write(path: FilePath, data: List[String]): F[Unit]
  def sink(path: FilePath): Sink[Task, String]
}

trait StoreUnsafe[F[_]] extends StoreUnsafeRead[F] with StoreUnsafeWrite[F]

trait StoreUnsafeRead[F[_]] {
  def withInputStream(path: FilePath)(f: InputStream => F[Unit]): F[Unit]
}

trait StoreUnsafeWrite[F[_]] {
  def withOutputStream(path: FilePath)(f: OutputStream => F[Unit]): F[Unit]
}
