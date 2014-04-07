package com.ambiata.mundane.store

import com.ambiata.mundane.io._
import java.io.{InputStream, OutputStream}
import scala.io.Codec
import scalaz._, Scalaz._, scalaz.stream._, scalaz.concurrent._
import scodec.bits.ByteVector

trait Store[F[_]] {
  def list(root: FilePath): F[List[FilePath]]

  def filter(root: FilePath, predicate: FilePath => Boolean): F[List[FilePath]]
  def find(root: FilePath, predicate: FilePath => Boolean): F[Option[FilePath]]
  def exists(root: FilePath): F[Boolean]

  def delete(path: FilePath): F[Unit]
  def deleteAll(root: FilePath): F[Unit]
  def deleteWhere(root: FilePath, predicate: FilePath => Boolean): F[Unit]
  def deleteWhereM(root: FilePath, predicate: FilePath => F[Boolean]): F[Unit]

  def move(in: FilePath, out: FilePath)
  def copy(in: FilePath, out: FilePath)
  def mirror(in: FilePath, out: FilePath)

  def moveTo(store: Store[F], in: FilePath, out: FilePath)
  def copyTo(store: Store[F], in: FilePath, out: FilePath)
  def mirrorTo(store: Store[F], in: FilePath, out: FilePath)

  def bytes: StoreBytes[F]
  def strings: StoreStrings[F]
  def utf8: StoreUtf8[F]
  def lines: StoreLines[F]
  def linesUtf8: StoreLines[F]
  def unsafe: StoreUnsafe[F]

  def checksum(path: FilePath, algorithm: ChecksumAlgorithm): F[Checksum]
}

trait StoreBytes[F[_]] {
  def read(path: FilePath): F[ByteVector]
  def write(path: FilePath, data: ByteVector): F[Unit]

  def source(path: FilePath): Process[Task, ByteVector]
  def sink(path: FilePath): Sink[Task, ByteVector]
}

trait StoreStrings[F[_]] {
  def read(path: FilePath, codec: Codec): F[String]
  def write(path: FilePath, data: String, codec: Codec): F[Unit]

  def source(path: FilePath, codec: Codec): Process[Task, String]
  def sink(path: FilePath, codec: Codec): Sink[Task, String]
}

trait StoreUtf8[F[_]] {
  def read(path: FilePath): F[String]
  def write(path: FilePath, data: String): F[Unit]

  def source(path: FilePath): Process[Task, String]
  def sink(path: FilePath): Sink[Task, String]
}

trait StoreLines[F[_]] {
  def read(path: FilePath, codec: Codec): F[List[String]]
  def write(path: FilePath, data: List[String], codec: Codec): F[Unit]

  def source(path: FilePath, codec: Codec): Process[Task, String]
  def sink(path: FilePath, codec: Codec): Sink[Task, String]
}

trait StoreLinesUtf8[F[_]] {
  def read(path: FilePath): F[List[String]]
  def write(path: FilePath, data: List[String]): F[Unit]

  def source(path: FilePath): Process[Task, String]
  def sink(path: FilePath): Sink[Task, String]
}

trait StoreUnsafe[F[_]] {
  def withInputStream(path: FilePath)(f: InputStream => F[Unit]): F[Unit]
  def withOutputStream(path: FilePath)(f: OutputStream => F[Unit]): F[Unit]
}
