package com.ambiata.mundane.store

import scalaz.{Store => _, _}, Scalaz._, \&/._, effect.IO
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
  copy                                            $copy
  mirror                                          $mirror

  moveTo                                          $moveTo
  copyTo                                          $copyTo
  mirrorTo                                        $mirrorTo

  checksum                                        $checksum

  read bytes                                      $readBytes
  write bytes                                     $writeBytes

  read strings                                    $readStrings
  write strings                                   $writeStrings

  read utf8 strings                               $readUtf8Strings
  write utf8 strings                              $writeUtf8Strings

  read lines                                      $readLines
  write lines                                     $writeLines

  read utf8 lines                                 $readUtf8Lines
  write utf8 lines                                $writeUtf8Lines

  read unsafe                                     $readUnsafe
  write unsafe                                    $writeUnsafe

  """

  val tmp = System.getProperty("java.io.tmpdir", "/tmp") </> s"StoreSpec.${UUID.randomUUID}"
  val store = PosixStore(tmp)

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
    prop((m: Entry, n: Entry) => clean(Paths(m)) { _ =>

      (store.deleteAll(FilePath.root) >> filepaths.traverseU(store.exists)) must beOkLike(x => !x.tail.exists(identity)) })
    pending

  def copy =
    pending

  def mirror =
    pending

  def moveTo =
    pending

  def copyTo =
    pending

  def mirrorTo =
    pending

  def checksum =
    pending

  def readBytes =
    pending

  def writeBytes =
    pending

  def readStrings =
    pending

  def writeStrings =
    pending

  def readUtf8Strings =
    pending

  def writeUtf8Strings =
    pending

  def readLines =
    pending

  def writeLines =
    pending

  def readUtf8Lines =
    pending

  def writeUtf8Lines =
    pending

  def readUnsafe =
    pending

  def writeUnsafe =
    pending

  def files(paths: Paths): List[FilePath] =
    paths.entries.map(e => e.path.toFilePath </> e.value.toString).sortBy(_.path)

  def create(paths: Paths): ResultT[IO, Unit] =
    paths.entries.traverseU(e =>
      Files.write(tmp </> e.path </> e.value.toString, e.value.toString)).void

  def clean[A](paths: Paths)(run: List[FilePath] => A): A = {
    create(paths).run.unsafePerformIO
    try run(files(paths))
    finally Directories.delete(tmp).run.unsafePerformIO
  }
}
