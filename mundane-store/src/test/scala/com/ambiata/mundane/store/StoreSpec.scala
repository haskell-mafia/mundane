package com.ambiata.mundane.store

import scala.io.Codec
import scalaz.{Store => _, _}, Scalaz._, \&/._, effect.IO
import scodec.bits.ByteVector
import org.specs2._
import com.ambiata.mundane.control._
import com.ambiata.mundane.io._
import com.ambiata.mundane.testing._, ResultTIOMatcher._
import java.io.{File, FileOutputStream, ByteArrayInputStream}
import java.util.UUID


class StoreSpec extends Specification with ScalaCheck { def is = isolated ^ s2"""
  Store Usage
  ==========

  list path                                       $list
  filter listed paths                             $filter
  find path in root (thirdish)                    $find
  find path in root (first)                       $findfirst
  find path in root (last)                        $findlast

  exists                                          $exists
  not exists                                      $notExists

  delete                                          $delete
  deleteAll                                       $deleteAll

  move                                            $move
  move and read                                   $moveRead
  copy                                            $copy
  copy and read                                   $copyRead
  mirror                                          $mirror

  moveTo                                          $moveTo
  copyTo                                          $copyTo
  mirrorTo                                        $mirrorTo

  checksum                                        $checksum

  read / write bytes                              $bytes

  read / write strings                            $strings

  read / write utf8 strings                       $utf8Strings

  read / write lines                              $lines

  read / write utf8 lines                         $utf8Lines

  """

  val tmp1 = System.getProperty("java.io.tmpdir", "/tmp") </> s"StoreSpec.${UUID.randomUUID}"
  val tmp2 = System.getProperty("java.io.tmpdir", "/tmp") </> s"StoreSpec.${UUID.randomUUID}"
  val store = PosixStore(tmp1)
  val alternate = PosixStore(tmp2)

  def list =
    prop((paths: Paths) => clean(paths) { filepaths =>
       store.list(FilePath.root) must beOkValue(filepaths) })

  def filter =
    prop((paths: Paths) => clean(paths) { filepaths =>
      val first = filepaths.head
      val last = filepaths.last
      val expected = if (first == last) List(first) else List(first, last)
      store.filter(FilePath.root, x => x == first || x == last) must beOkLike(paths => paths must contain(allOf(expected:_*))) })

  def find =
    prop((paths: Paths) => paths.entries.length >= 3 ==> { clean(paths) { filepaths =>
      val third = filepaths.drop(2).head
      store.find(FilePath.root, _ == third) must beOkValue(Some(third)) } })

  def findfirst =
    prop((paths: Paths) => clean(paths) { filepaths =>
      store.find(FilePath.root, x => x == filepaths.head) must beOkValue(Some(filepaths.head)) })

  def findlast =
    prop((paths: Paths) => clean(paths) { filepaths =>
      store.find(FilePath.root, x => x == filepaths.last) must beOkValue(Some(filepaths.last)) })

  def exists =
    prop((paths: Paths) => clean(paths) { filepaths =>
      filepaths.traverseU(store.exists) must beOkLike(_.forall(identity)) })

  def notExists =
    prop((paths: Paths) => store.exists(FilePath.root </> "i really don't exist") must beOkValue(false))

  def delete =
    prop((paths: Paths) => clean(paths) { filepaths =>
      val first = filepaths.head
      (store.delete(first) >> filepaths.traverseU(store.exists)) must beOkLike(x => !x.head && x.tail.forall(identity)) })

  def deleteAll =
    prop((paths: Paths) => clean(paths) { filepaths =>
      (store.deleteAll(FilePath.root) >> filepaths.traverseU(store.exists)) must beOkLike(x => !x.tail.exists(identity)) })

  def move =
    prop((m: Entry, n: Entry) => clean(Paths(m :: Nil)) { _ =>
      (store.move(m.full.toFilePath, n.full.toFilePath) >>
       store.exists(m.full.toFilePath).zip(store.exists(n.full.toFilePath))) must beOkValue(false -> true) })

  def moveRead =
    prop((m: Entry, n: Entry) => clean(Paths(m :: Nil)) { _ =>
      (store.move(m.full.toFilePath, n.full.toFilePath) >>
       store.utf8.read(n.full.toFilePath)) must beOkValue(m.value.toString) })

  def copy =
    prop((m: Entry, n: Entry) => clean(Paths(m :: Nil)) { _ =>
      (store.copy(m.full.toFilePath, n.full.toFilePath) >>
       store.exists(m.full.toFilePath).zip(store.exists(n.full.toFilePath))) must beOkValue(true -> true) })

  def copyRead =
    prop((m: Entry, n: Entry) => clean(Paths(m :: Nil)) { _ =>
      (store.copy(m.full.toFilePath, n.full.toFilePath) >>
       store.utf8.read(m.full.toFilePath).zip(store.utf8.read(n.full.toFilePath))) must beOkLike({ case (in, out) => in must_== out }) })

  def mirror =
    prop((paths: Paths) => clean(paths) { filepaths =>
      store.mirror(FilePath.root, FilePath.root </> "mirror") >> store.list(FilePath.root </> "mirror") must beOkValue(filepaths.map("mirror" </> _)) })

  def moveTo =
    prop((m: Entry, n: Entry) => clean(Paths(m :: Nil)) { _ =>
      (store.moveTo(alternate, m.full.toFilePath, n.full.toFilePath) >>
       store.exists(m.full.toFilePath).zip(alternate.exists(n.full.toFilePath))) must beOkValue(false -> true) })

  def copyTo =
    prop((m: Entry, n: Entry) => clean(Paths(m :: Nil)) { _ =>
      (store.copyTo(alternate, m.full.toFilePath, n.full.toFilePath) >>
       store.exists(m.full.toFilePath).zip(alternate.exists(n.full.toFilePath))) must beOkValue(true -> true) })

  def mirrorTo =
    prop((paths: Paths) => clean(paths) { filepaths =>
      store.mirrorTo(alternate, FilePath.root, FilePath.root </> "mirror") >> alternate.list(FilePath.root </> "mirror") must beOkValue(filepaths.map("mirror" </> _)) })

  def checksum =
    prop((m: Entry) => clean(Paths(m :: Nil)) { _ =>
      store.checksum(m.full.toFilePath, MD5) must beOkValue(Checksum.string(m.value.toString, MD5)) })

  def bytes =
    prop((m: Entry, bytes: Array[Byte]) => clean(Paths(m :: Nil)) { _ =>
      (store.bytes.write(m.full.toFilePath, ByteVector(bytes)) >> store.bytes.read(m.full.toFilePath)) must beOkValue(ByteVector(bytes)) })

  def strings =
    prop((m: Entry, s: String) => clean(Paths(m :: Nil)) { _ =>
      (store.strings.write(m.full.toFilePath, s, Codec.UTF8) >> store.strings.read(m.full.toFilePath, Codec.UTF8)) must beOkValue(s) })

  def utf8Strings =
    prop((m: Entry, s: String) => clean(Paths(m :: Nil)) { _ =>
      (store.utf8.write(m.full.toFilePath, s) >> store.utf8.read(m.full.toFilePath)) must beOkValue(s) })

  def lines =
    prop((m: Entry, s: List[Int]) => clean(Paths(m :: Nil)) { _ =>
      (store.lines.write(m.full.toFilePath, s.map(_.toString), Codec.UTF8) >> store.lines.read(m.full.toFilePath, Codec.UTF8)) must beOkValue(s.map(_.toString)) })

  def utf8Lines =
    prop((m: Entry, s: List[Int]) => clean(Paths(m :: Nil)) { _ =>
      (store.linesUtf8.write(m.full.toFilePath, s.map(_.toString)) >> store.linesUtf8.read(m.full.toFilePath)) must beOkValue(s.map(_.toString)) })

  def files(paths: Paths): List[FilePath] =
    paths.entries.map(e => e.full.toFilePath).sortBy(_.path)

  def create(paths: Paths): ResultT[IO, Unit] =
    paths.entries.traverseU(e =>
      Files.write(tmp1 </> e.full, e.value.toString)).void

  def clean[A](paths: Paths)(run: List[FilePath] => A): A = {
    create(paths).run.unsafePerformIO
    try run(files(paths))
    finally (Directories.delete(tmp1) >> Directories.delete(tmp2)).run.unsafePerformIO
  }
}
