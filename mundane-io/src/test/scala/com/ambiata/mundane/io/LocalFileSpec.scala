package com.ambiata.mundane.io

import com.ambiata.disorder._
import com.ambiata.mundane.data._
import com.ambiata.mundane.path._
import com.ambiata.mundane.path.Arbitraries._
import com.ambiata.mundane.control._
import com.ambiata.mundane.io.Arbitraries._
import com.ambiata.mundane.testing.RIOMatcher._
import java.io.File
import java.net.URI

import org.specs2._
import org.scalacheck._
import scala.io.Codec
import scalaz._, Scalaz._, effect.Effect._

class LocalFileSpec extends Specification with ScalaCheck { def is = s2"""

 Paths are of 2 sorts:

  - file paths
  - directory paths

 The essential difference is only directory paths can be appended with a path name and become a LocalFile.

 They also map to the notion of file and directory on a filesystem (but not on the Java notion of File which can represent both)

 Path
 ====

  'LocalFile.fromFile' is symmetric with Path#toFile:

    ${ prop((p: Path) => LocalFile.fromFile(p.toFile).path ==== p) }

  'LocalFile.fromFile' is consistent with Path.apply:

    ${ prop((p: Path) => LocalFile.fromFile(new java.io.File(p.path)).path ==== p) }

  'LocalFile.toLocalPath' is symmetric with Path#determine

    ${ prop((l: LocalTemporary) => for { f <- l.file; r <- f.toLocalPath.determine } yield r ==== -\/[LocalFile](f).some) }


 LocalFiles
 ==========

 A LocalFile can be created from
   a String
   ${ LocalFile.unsafe("hello/world").path.path === "hello/world"  }

   a File
   ${ LocalFile.fromFile(new File("hello/world")).path.path === "hello/world" }

   a URI
   ${ LocalFile.fromURI(new URI("hello/world")).map(_.path.path) === Some("hello/world")  }

   get the path as a string
   ${ LocalFile.unsafe("test").path.path must_== "test" }
   ${ (LocalDirectory.Relative /- "test" /- "hello" /- "world").path must_== "test/hello/world" }
   ${ (LocalDirectory.Relative /- "test" /- "hello" / LocalDirectory.Relative).path must_== "test/hello" }

   filter hidden files from a list
   ${ List(LocalFile.unsafe("hello/.world"), LocalFile.unsafe("hello/world"), LocalFile.unsafe("hello/_SUCCESS")).filterHidden === List(LocalFile.unsafe("hello/world")) }

 A LocalFile can be ordered
   ${ List(LocalFile.unsafe("z"), LocalFile.unsafe("a")).sorted ==== List(LocalFile.unsafe("a"), LocalFile.unsafe("z")) }

  IO
  ==

  LocalFile should be able to perform these basic operations
    exists
    ${ LocalTemporary.random.file.flatMap(_.exists) must beOkValue(true) }

    unless exists
    ${ var i = 0; LocalFile.unsafe("test").unlessExists("", RIO.io({ i = 1; i })) must beOkValue(1) }

    delete
    ${ LocalTemporary.random.file.flatMap(f => f.delete >> f.exists) must beOkValue(false) }

  LocalFile should be able to read
    read
    ${ prop((s: S, l: LocalTemporary) => l.fileWithContent(s.value).flatMap(_.read) must beOkValue(s.value.some)) }

    read with encoding
    ${ prop((c: Codec, s: S, l: LocalTemporary) => validForCodec(s, c) ==> (
         l.path.flatMap(_.writeWithEncoding(s.value, c).flatMap(_.readWithEncoding(c))) must beOkValue(s.value.some))) }

    read lines
    ${ prop((s: List[S], l: LocalTemporary) =>
         l.path.flatMap(_.writeLines(s.map(noNewLines)).flatMap(_.readLines)) must beOkValue(s.map(noNewLines).some) )}

    read unsafe
    $readUnsafe

    do per line
    $doPerLine

    read per line
    $readPerLine

    read lines with encoding
    ${ prop((s: List[S], c: Codec, l: LocalTemporary) => s.map(validForCodec(_, c)).suml ==> (
         l.path.flatMap(_.writeLinesWithEncoding(s.map(noNewLines), c).flatMap(_.readLinesWithEncoding(c))) must beOkValue(s.map(noNewLines).some)) )}

    read bytes
    ${ prop((bs: Array[Byte], l: LocalTemporary) => l.path.flatMap(p => p.writeBytes(bs).flatMap(f => f.readBytes.map(_.map(_.toList)))) must
         beOkValue(bs.toList.some)) }

    read or fail
    ${ prop((s: S, l: LocalTemporary) => l.fileWithContent(s.value).flatMap(_.readOrFail) must beOkValue(s.value)) }

    handle failure
    ${ prop((l: LocalTemporary) => l.file.flatMap(f => f.delete >> f.read) must beOkLike(_ must beNone)) }
    ${ prop((l: LocalTemporary) => l.file.flatMap(f => f.delete >> f.readOrFail) must beFailWithMessage("Failed to read file - file does not exist")) }

  LocalFile should be able to append content to files that exists
    append
    ${ prop((d: DistinctPair[S], l: LocalTemporary) => l.path.flatMap(_.write(d.first.value)).flatMap(f => f.append(d.second.value) >> f.read) must
         beOkValue((d.first.value ++ d.second.value).some)) }

    append with encoding
    $appendWithEncoding

    append lines
    ${ prop((i: List[S], s: List[S], l: LocalTemporary) => l.path.flatMap(_.writeLines(i.map(noNewLines))).flatMap(f =>
         f.appendLines(s.map(noNewLines)) >> f.readLines) must beOkValue((i ++ s).map(noNewLines).some)) }

    append lines with encoding
    $appendLinesWithEncoding

    append bytes
    ${ prop((i: Array[Byte], s: Array[Byte], l: LocalTemporary) => l.path.flatMap(_.writeBytes(i)).flatMap(f =>
         f.appendBytes(s) >> f.readBytes.map(_.map(_.toList))) must beOkValue((i ++ s).toList.some)) }

  LocalFile should be able to write content using a stream
    $stream

  LocalFile should be able to overwrite content in files
    overwrite
    ${ prop((d: DistinctPair[S], l: LocalTemporary) => l.path.flatMap(_.write(d.first.value)).flatMap(f => f.overwrite(d.second.value) >> f.read) must
         beOkValue(d.second.value.some)) }

    overwrite with encoding
    $overwriteWithEncoding

    overwrite lines
    ${ prop((i: S, s: List[S], l: LocalTemporary) => l.path.flatMap(_.write(i.value)).flatMap(f =>
         f.overwriteLines(s.map(noNewLines)) >> f.readLines) must beOkValue(s.map(noNewLines).some)) }

    overwirte lines with encoding
    $overwriteLinesWithEncoding

    overwrite bytes
    ${ prop((i: Array[Byte], s: Array[Byte], l: LocalTemporary) => l.path.flatMap(_.writeBytes(i)).flatMap(f =>
         f.overwriteBytes(s) >> f.readBytes.map(_.map(_.toList))) must beOkValue((s).toList.some)) }

  LocalFile should be able to move
    move
    $move

    move to
//

    handle failure
    $moveSourceFailure
    $moveDestinationFailure

  LocalFile should be able to copy
    copy
    $copy

    copy to
//

    handle failure
    $copySourceFailure
    $copyDestinationFailure

"""
  def readUnsafe = prop((s: S, l: LocalTemporary) => {
    var x: String = "";
    l.fileWithContent(s.value).flatMap(_.readUnsafe(in => for {
      v <- Streams.read(in)
      _ <- RIO.safe(x = v)
    } yield ())).as(x)
  } must beOkValue(s.value))

  def doPerLine = prop((list: List[S], l: LocalTemporary) => {
    var i = scala.collection.mutable.ListBuffer[String]()
    l.path.flatMap(_.writeLines(list.map(noNewLines))).flatMap(_.doPerLine(s =>
      RIO.safe(i += s))).as(i.toList)
  } must beOkValue(list.map(noNewLines)))

  def readPerLine = prop((list: List[S], l: LocalTemporary) => for {
    p <- l.path
    f <- p.writeLines(list.map(noNewLines))
    r <- f.readPerLine(scala.collection.mutable.ListBuffer[String]())((s, b) => { b +=s; b})
  } yield r.toList ==== list.map(noNewLines))

  def appendWithEncoding = prop((d: DistinctPair[S], c: Codec, l: LocalTemporary) =>
    (validForCodec(d.first, c) && validForCodec(d.second, c)) ==> (for {
      p <- l.path
      f <- p.writeWithEncoding(d.first.value, c)
      _ <- f.appendWithEncoding(d.second.value, c)
      r <- f.readWithEncoding(c)
    } yield r ==== (d.first.value ++ d.second.value).some))

  def appendLinesWithEncoding = prop((i: List[S], s: List[S], c: Codec, l: LocalTemporary) =>
    (i.map(validForCodec(_, c)) ++ s.map(validForCodec(_, c))).suml ==> (for {
      p <- l.path
      f <- p.writeLinesWithEncoding(i.map(noNewLines), c)
      _ <- f.appendLinesWithEncoding(s.map(noNewLines), c)
      r <- f.readLinesWithEncoding(c)
    } yield r ==== (i ++ s).map(noNewLines).some))

  def stream = prop((s: S, l: LocalTemporary) => for {
    a <- l.fileWithContent(s.value)
    b <- l.file
    _ <- RIO.using(a.toInputStream)(in => b.writeStream(in))
    r <- b.readOrFail
  } yield r ==== s.value)

  def overwriteWithEncoding = prop((d: DistinctPair[S], c: Codec, l: LocalTemporary) =>
    (validForCodec(d.first, c) && validForCodec(d.second, c)) ==> (for {
      p <- l.path
      f <- p.writeWithEncoding(d.first.value, c)
      _ <- f.overwriteWithEncoding(d.second.value, c)
      r <- f.readWithEncoding(c)
    } yield r ==== d.second.value.some))

  def overwriteLinesWithEncoding = prop((i: List[S], s: List[S], c: Codec, l: LocalTemporary) =>
    (i.map(validForCodec(_, c)) ++ s.map(validForCodec(_, c))).suml ==> (for {
      p <- l.path
      f <- p.writeLinesWithEncoding(i.map(noNewLines), c)
      _ <- f.overwriteLinesWithEncoding(s.map(noNewLines), c)
      r <- f.readLinesWithEncoding(c)
    } yield r ==== s.map(noNewLines).some))

  def move = prop((s: S, l: LocalTemporary) => for {
    p <- l.path
    f <- l.fileWithContent(s.value)
    n <- f.move(p)
    e <- f.exists
    r <- LocalFile.unsafe(p.path.path).readOrFail
  } yield e -> r ==== false -> s.value)

  def moveSourceFailure = prop((s: S, l: LocalTemporary) => (for {
    p <- l.path
    _ <- p.touch
    f <- l.fileWithContent(s.value)
    _ <- f.delete
    _ <- f.move(p)
  } yield ()) must beFail)

  def moveDestinationFailure = prop((s: S, l: LocalTemporary) => (for {
    p <- l.path
    _ <- p.touch
    f <- l.fileWithContent(s.value)
    _ <- f.move(p)
  } yield ()) must beFail)

  def copy = prop((s: S, l: LocalTemporary) => for {
    p <- l.path
    f <- l.fileWithContent(s.value)
    n <- f.copy(p)
    e <- f.exists
    r <- n.readOrFail
  } yield e -> r ==== true -> s.value)

  def copySourceFailure = prop((s: S, l: LocalTemporary) => (for {
    p <- l.path
    _ <- p.touch
    f <- l.fileWithContent(s.value)
    _ <- f.delete
    _ <- f.copy(p)
  } yield ()) must beFail)

  def copyDestinationFailure = prop((s: S, l: LocalTemporary) => (for {
    p <- l.path
    _ <- p.touch
    f <- l.fileWithContent(s.value)
    _  <- f.copy(p)
  } yield ()) must beFail)

  def validForCodec(s: S, c: Codec): Boolean =
    new String(s.value.getBytes(c.name), c.name) == s.value

  def noNewLines(s: S): String =
    s.value.replaceAll("\\s", "")

  implicit val BooleanMonoid: Monoid[Boolean] =
    scalaz.std.anyVal.booleanInstance.conjunction
}
