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

// FIX Workout how this test can be pulled out and shared with posix/s3/hdfs.
class PosixStoreSpec extends Specification with ScalaCheck { def is = isolated ^ s2"""
  Posix Store Usage
  =================

  list all file paths                             $list
  list all file paths from a sub path             $listSubPath
  list directories                                $listDirs
  list directories from a sub path                $listDirsSubPath
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

  val tmp1 = DirPath.unsafe(System.getProperty("java.io.tmpdir", "/tmp")) </> FileName.unsafe(s"StoreSpec.${UUID.randomUUID}")
  val tmp2 = DirPath.unsafe(System.getProperty("java.io.tmpdir", "/tmp")) </> FileName.unsafe(s"StoreSpec.${UUID.randomUUID}")
  val store = PosixStore(tmp1)
  val alternate = PosixStore(tmp2)

  def list =
    prop((paths: Paths) => clean(paths) { filepaths =>
       store.listAll must beOkLike((_:List[FilePath]).toSet must_== filepaths.toSet) })

  def listSubPath =
    prop((paths: Paths) => clean(paths.map(_ prepend "sub")) { filepaths =>
      store.list(DirPath.Empty </> "sub") must beOkLike((_:List[FilePath]).toSet must_== filepaths.map(_.fromRoot).toSet) })

  def listDirs =
    prop((paths: Paths) => clean(paths) { filepaths =>
      store.listDirs(DirPath.Empty) must beOkLike((_:List[DirPath]).toSet must_== filepaths.map(_.rootname).toSet) })

  def listDirsSubPath =
    prop((paths: Paths) => clean(paths.map(_ prepend "sub")) { filepaths =>
      store.listDirs(DirPath.Empty </> "sub") must beOkLike((_:List[DirPath]).toSet must_== filepaths.map(_.fromRoot.rootname).toSet)
    })

  def filter =
    prop((paths: Paths) => clean(paths) { filepaths =>
      val first = filepaths.head
      val last = filepaths.last
      val expected = if (first == last) List(first) else List(first, last)
      store.filterAll(x => x == first || x == last) must beOkLike(paths => paths must contain(allOf(expected:_*))) })

  def find =
    prop((paths: Paths) => paths.entries.length >= 3 ==> { clean(paths) { filepaths =>
      val third = filepaths.drop(2).head
      store.findAll(_ == third) must beOkValue(Some(third)) } })

  def findfirst =
    prop((paths: Paths) => clean(paths) { filepaths =>
      store.findAll(x => x == filepaths.head) must beOkValue(Some(filepaths.head)) })

  def findlast =
    prop((paths: Paths) => clean(paths) { filepaths =>
      store.findAll(x => x == filepaths.last) must beOkValue(Some(filepaths.last)) })

  def exists =
    prop((paths: Paths) => clean(paths) { filepaths =>
      filepaths.traverseU(store.exists) must beOkLike(_.forall(identity)) })

  def notExists =
    prop((paths: Paths) => store.exists(DirPath.unsafe("root") </> "i really don't exist") must beOkValue(false))

  def delete =
    prop((paths: Paths) => clean(paths) { filepaths =>
      val first = filepaths.head
      (store.delete(first) >> filepaths.traverseU(store.exists)) must beOkLike(x => !x.head && x.tail.forall(identity)) })

  def deleteAll =
    prop((paths: Paths) => clean(paths) { filepaths =>
      (store.deleteAllFromRoot >> filepaths.traverseU(store.exists)) must beOkLike(x => !x.tail.exists(identity)) })

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
      store.mirror(DirPath.Root, DirPath.unsafe("mirror")) >> store.list(DirPath.unsafe("mirror")) must
        beOkLike((_:List[FilePath]).toSet must_== filepaths.toSet) })

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
      store.mirrorTo(alternate, DirPath.Root, DirPath.unsafe("mirror")) >> alternate.list(DirPath.unsafe("mirror")) must
        beOkLike((_:List[FilePath]).toSet must_== filepaths.toSet) })

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
    paths.entries.map(e => e.full.toFilePath.asRelative).sortBy(_.path)

  def create(paths: Paths): ResultT[IO, Unit] =
    paths.entries.traverseU(e =>
      Files.write(tmp1 </> FilePath.unsafe(e.full), e.value.toString)).void

  def clean[A](paths: Paths)(run: List[FilePath] => A): A = {
    create(paths).run.unsafePerformIO
    try run(files(paths))
    finally (Directories.delete(tmp1) >> Directories.delete(tmp2)).run.unsafePerformIO
  }

  implicit class ToDirPath(s: String) {
    def toDirPath: DirPath = DirPath.unsafe(s)
  }

  implicit class ToFilePath(s: String) {
    def toFilePath: FilePath = FilePath.unsafe(s)
  }

}
