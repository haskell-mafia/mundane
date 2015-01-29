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

     ${ List(LocalFile.unsafe("hello/.world"), LocalFile.unsafe("hello/world"), LocalFile.unsafe("hello/_SUCCESS")).filterHidden ===
          List(LocalFile.unsafe("hello/world")) }

 A LocalFile can be ordered

   ${ List(LocalFile.unsafe("z"), LocalFile.unsafe("a")).sorted ==== List(LocalFile.unsafe("a"), LocalFile.unsafe("z")) }

  IO
  ==

  LocalFile should be able to perform these basic operations

    ${ LocalTemporary.random.file.flatMap(_.exists) must beOkValue(true) }

    ${ var i = 0; LocalTemporary.random.file.flatMap(_.whenExists(RIO.io({ i = 1; i }).void)).as(i) must beOkValue(1) }

    ${ var i = 0; LocalTemporary.random.file.flatMap(_.doesExist("", RIO.io({ i = 1; i }))) must beOkValue(1) }

    ${ var i = 0; LocalTemporary.random.file.flatMap(_.doesNotExist("", RIO.io({ i = 1; i }))) must beFail }

    ${ var i = 0; LocalFile.unsafe("test").doesNotExist("", RIO.io({ i = 1; i })) must beOkValue(1) }

    ${ LocalTemporary.random.file.flatMap(f => f.delete >> f.exists) must beOkValue(false) }


  LocalFile should be able to read different content from files using different methods

    ${ prop((s: S, l: LocalTemporary) => l.fileWithContent(s.value).flatMap(_.read) must beOkValue(s.value.some)) }

    ${ prop((c: Codec, s: S, l: LocalTemporary) => validForCodec(s, c) ==> (
         l.path.flatMap(_.writeWithEncoding(s.value, c).flatMap(_.readWithEncoding(c))) must beOkValue(s.value.some))) }

    ${ prop((s: List[S], l: LocalTemporary) =>
         l.path.flatMap(_.writeLines(s.map(noNewLines)).flatMap(_.readLines)) must beOkValue(s.map(noNewLines).some) )}

    ${ prop((s: S, l: LocalTemporary) => {
         var x: String = "";
         for {
           f <- l.fileWithContent(s.value)
           _ <- f.readUnsafe(in => for {
             v <- Streams.read(in)
             _ <- RIO.safe(x = v)
           } yield ())
         } yield x ==== s.value
       })
     }

    ${ prop((list: List[S], l: LocalTemporary) => {
         var i = scala.collection.mutable.ListBuffer[String]()
         for {
           p <- l.path
           f <- p.writeLines(list.map(noNewLines))
           r <- f.doPerLine(s =>
             RIO.safe(i += s))
         } yield i.toList ==== list.map(noNewLines)
       })
     }

   ${ prop((list: List[S], l: LocalTemporary) => for {
         p <- l.path
         f <- p.writeLines(list.map(noNewLines))
         r <- f.readPerLine(scala.collection.mutable.ListBuffer[String]())((s, b) => { b +=s; b})
       } yield r.toList ==== list.map(noNewLines))
     }

     ${ prop((s: List[S], c: Codec, l: LocalTemporary) => s.map(validForCodec(_, c)).suml ==> (
         l.path.flatMap(_.writeLinesWithEncoding(s.map(noNewLines), c).flatMap(_.readLinesWithEncoding(c))) must beOkValue(s.map(noNewLines).some)) )}

    ${ prop((bs: Array[Byte], l: LocalTemporary) => l.path.flatMap(p => p.writeBytes(bs).flatMap(f => f.readBytes.map(_.map(_.toList)))) must
         beOkValue(bs.toList.some)) }

    ${ prop((s: S, l: LocalTemporary) => l.fileWithContent(s.value).flatMap(_.readOrFail) must beOkValue(s.value)) }

   Handle failure cases

    ${ prop((l: LocalTemporary) => l.file.flatMap(f => f.delete >> f.read) must beOkLike(_ must beNone)) }

    ${ prop((l: LocalTemporary) => l.file.flatMap(f => f.delete >> f.readOrFail) must beFail) }


  LocalFile should be able to append different content to files that exist

    ${ prop((d: DistinctPair[S], l: LocalTemporary) => l.path.flatMap(_.write(d.first.value)).flatMap(f => f.append(d.second.value) >> f.read) must
         beOkValue((d.first.value ++ d.second.value).some)) }

    ${ prop((d: DistinctPair[S], c: Codec, l: LocalTemporary) => (validForCodec(d.first, c) && validForCodec(d.second, c)) ==> (for {
         p <- l.path
         f <- p.writeWithEncoding(d.first.value, c)
         _ <- f.appendWithEncoding(d.second.value, c)
         r <- f.readWithEncoding(c)
       } yield r ==== (d.first.value ++ d.second.value).some))
     }

    ${ prop((i: List[S], s: List[S], l: LocalTemporary) => l.path.flatMap(_.writeLines(i.map(noNewLines))).flatMap(f =>
         f.appendLines(s.map(noNewLines)) >> f.readLines) must beOkValue((i ++ s).map(noNewLines).some)) }

    ${ prop((i: List[S], s: List[S], c: Codec, l: LocalTemporary) => (i.map(validForCodec(_, c)) ++ s.map(validForCodec(_, c))).suml ==> (for {
         p <- l.path
         f <- p.writeLinesWithEncoding(i.map(noNewLines), c)
         _ <- f.appendLinesWithEncoding(s.map(noNewLines), c)
         r <- f.readLinesWithEncoding(c)
       } yield r ==== (i ++ s).map(noNewLines).some))
     }

    ${ prop((i: Array[Byte], s: Array[Byte], l: LocalTemporary) => l.path.flatMap(_.writeBytes(i)).flatMap(f =>
         f.appendBytes(s) >> f.readBytes.map(_.map(_.toList))) must beOkValue((i ++ s).toList.some)) }


  LocalFile should be able to write different content to files using different Encodings and Mode's.
  Content includes streams, strings, lines and bytes.

    ${ prop((s: S, l: LocalTemporary) => for {
         a <- l.fileWithContent(s.value)
         b <- l.file
         _ <- RIO.using(a.toInputStream)(in => b.writeStream(in))
         r <- b.readOrFail
       } yield r ==== s.value)
     }

    Can write to a file with different mode's.

      ${ prop((s: DistinctPair[S], l: LocalTemporary) => for {
           f <- l.fileWithContent(s.first.value)
           _ <- f.writeWithMode(s.second.value, WriteMode.Append)
           r <- f.readOrFail
         } yield r ==== (s.first.value ++ s.second.value))
       }

      ${ prop((s: DistinctPair[S], l: LocalTemporary) => for {
           f <- l.fileWithContent(s.first.value)
           _ <- f.writeWithMode(s.second.value, WriteMode.Overwrite)
           r <- f.readOrFail
         } yield r ==== s.second.value)
       }

      ${ prop((s: DistinctPair[S], l: LocalTemporary) => (for {
           f <- l.fileWithContent(s.first.value)
           _ <- f.writeWithMode(s.second.value, WriteMode.Fail)
         } yield ()) must beFail)
       }

    Can write to files with different modes using different encodings

      ${ prop((s: DistinctPair[S], c: Codec, l: LocalTemporary) => pairForCodec(s, c) ==> (for {
           p <- l.path
           f <- p.writeWithEncoding(s.first.value, c)
           _ <- f.writeWithEncodingMode(s.second.value, c, WriteMode.Append)
           r <- f.readWithEncoding(c)
         } yield r ==== (s.first.value ++ s.second.value).some))
       }

      ${ prop((s: DistinctPair[S], c: Codec, l: LocalTemporary) => pairForCodec(s, c) ==> (for {
           p <- l.path
           f <- p.writeWithEncoding(s.first.value, c)
           _ <- f.writeWithEncodingMode(s.second.value, c, WriteMode.Overwrite)
           r <- f.readWithEncoding(c)
         } yield r ==== s.second.value.some))
       }

      ${ prop((s: DistinctPair[S], c: Codec, l: LocalTemporary) => pairForCodec(s, c) ==> { (for {
           p <- l.path
           f <- p.writeWithEncoding(s.first.value, c)
           _ <- f.writeWithEncodingMode(s.second.value, c, WriteMode.Fail)
         } yield ()) must beFail })
       }

    Can write lines to a file with different mode's.

      ${ prop((a: List[S], b: List[S], l: LocalTemporary) => for {
           p <- l.path
           f <- p.writeLines(a.map(noNewLines))
           _ <- f.writeLinesWithMode(b.map(noNewLines), WriteMode.Append)
           r <- f.readLines
         } yield r ==== (a ++ b).map(noNewLines).some)
       }

      ${ prop((a: List[S], b: List[S], l: LocalTemporary) => for {
           p <- l.path
           f <- p.writeLines(a.map(noNewLines))
           _ <- f.writeLinesWithMode(b.map(noNewLines), WriteMode.Overwrite)
           r <- f.readLines
         } yield r ==== (b.map(noNewLines).some))
       }

      ${ prop((a: List[S], b: List[S], l: LocalTemporary) => (for {
           p <- l.path
           f <- p.writeLines(a.map(noNewLines))
           _ <- f.writeLinesWithMode(b.map(noNewLines), WriteMode.Fail)
         } yield ()) must beFail)
       }


    Can write lines with different Codec's and Mode's

      ${ prop((a: List[S], b: List[S], c: Codec, l: LocalTemporary) => (a ++ b).map(validForCodec(_, c)).suml ==> (for {
           p <- l.path
           f <- p.writeLinesWithEncoding(a.map(noNewLines), c)
           _ <- f.writeLinesWithEncodingMode(b.map(noNewLines), c, WriteMode.Append)
           r <- f.readLinesWithEncoding(c)
         } yield r ==== (a ++ b).map(noNewLines).some))
       }

      ${ prop((a: List[S], b: List[S], c: Codec, l: LocalTemporary) => (a ++ b).map(validForCodec(_, c)).suml ==> (for {
           p <- l.path
           f <- p.writeLinesWithEncoding(a.map(noNewLines), c)
           _ <- f.writeLinesWithEncodingMode(b.map(noNewLines), c, WriteMode.Overwrite)
           r <- f.readLinesWithEncoding(c)
         } yield r ==== (b.map(noNewLines).some)))
       }

      ${ prop((a: List[S], b: List[S], c: Codec, l: LocalTemporary) => (a ++ b).map(validForCodec(_, c)).suml ==> { (for {
           p <- l.path
           f <- p.writeLinesWithEncoding(a.map(noNewLines), c)
           _ <- f.writeLinesWithEncodingMode(b.map(noNewLines), c, WriteMode.Fail)
         } yield ()) must beFail })
       }

    Can write with different Mode's

      ${ prop((a: Array[Byte], b: Array[Byte], l: LocalTemporary) => for {
           p <- l.path
           f <- p.writeBytes(a)
           _ <- f.writeBytesWithMode(b, WriteMode.Append)
           r <- f.readBytes
         } yield r.map(_.toList) ==== (a ++ b).toList.some)
       }

      ${ prop((a: Array[Byte], b: Array[Byte], l: LocalTemporary) => for {
           p <- l.path
           f <- p.writeBytes(a)
           _ <- f.writeBytesWithMode(b, WriteMode.Overwrite)
           r <- f.readBytes
         } yield r.map(_.toList) ==== b.toList.some)
       }

      ${ prop((a: Array[Byte], l: LocalTemporary) => (for {
           p <- l.path
           f <- p.writeBytes(a)
           _ <- f.writeBytesWithMode(a, WriteMode.Fail)
         } yield ()) must beFail)
       }


  LocalFile should be able to overwrite content in files

    ${ prop((d: DistinctPair[S], l: LocalTemporary) => l.path.flatMap(_.write(d.first.value)).flatMap(f => f.overwrite(d.second.value) >> f.read) must
         beOkValue(d.second.value.some)) }

      ${ prop((d: DistinctPair[S], c: Codec, l: LocalTemporary) => (validForCodec(d.first, c) && validForCodec(d.second, c)) ==> (for {
           p <- l.path
           f <- p.writeWithEncoding(d.first.value, c)
           _ <- f.overwriteWithEncoding(d.second.value, c)
           r <- f.readWithEncoding(c)
         } yield r ==== d.second.value.some))
       }

      ${ prop((i: S, s: List[S], l: LocalTemporary) => l.path.flatMap(_.write(i.value)).flatMap(f =>
           f.overwriteLines(s.map(noNewLines)) >> f.readLines) must beOkValue(s.map(noNewLines).some)) }

      ${ prop((i: List[S], s: List[S], c: Codec, l: LocalTemporary) => (i.map(validForCodec(_, c)) ++ s.map(validForCodec(_, c))).suml ==> (for {
           p <- l.path
           f <- p.writeLinesWithEncoding(i.map(noNewLines), c)
           _ <- f.overwriteLinesWithEncoding(s.map(noNewLines), c)
           r <- f.readLinesWithEncoding(c)
         } yield r ==== s.map(noNewLines).some))
       }

      ${ prop((i: Array[Byte], s: Array[Byte], l: LocalTemporary) => l.path.flatMap(_.writeBytes(i)).flatMap(f =>
           f.overwriteBytes(s) >> f.readBytes.map(_.map(_.toList))) must beOkValue((s).toList.some)) }

  LocalFile should be able to move files

    ${ prop((s: S, l: LocalTemporary) => for {
         p <- l.path
         f <- l.fileWithContent(s.value)
         n <- f.move(p)
         e <- f.exists
         r <- LocalFile.unsafe(p.path.path).readOrFail
       } yield e -> r ==== false -> s.value)
     }

    Move with mode and overwrite the target file

      ${ prop((s: S, l: LocalTemporary) => for {
           p <- l.path
           _ <- p.touch
           f <- l.fileWithContent(s.value)
           n <- f.moveWithMode(p, TargetMode.Overwrite)
           r <- n.readOrFail
         } yield r ==== s.value)
       }

    Move a file to a directory checking the path and contents

      ${ prop((s: S, l: LocalTemporary) => for {
           p <- l.path
           d <- l.directory
           f <- l.fileWithContent(s.value)
           n <- f.moveTo(d)
           a <- f.exists
           r <- n.readOrFail
         } yield a -> r ==== false -> s.value)
       }

      ${ prop((s: S, l: LocalTemporary) => for {
           p <- l.path
           d <- l.directory
           f <- p.write(s.value)
           n <- f.moveTo(d)
         } yield n.toLocalPath ==== (d.toLocalPath | p.basename.get))
       }

    Moving a file can fail, handle those failure cases.

      ${ prop((s: S, l: LocalTemporary) => (for {
           p <- l.path
           _ <- p.touch
           f <- l.fileWithContent(s.value)
           _ <- f.delete
           _ <- f.move(p)
         } yield ()) must beFail)
       }

      ${ prop((s: S, l: LocalTemporary) => (for {
           p <- l.path
           _ <- p.touch
           f <- l.fileWithContent(s.value)
           _ <- f.move(p)
         } yield ()) must beFail)
       }

  LocalFile should be able to copy files and handle copy failures

    ${ prop((s: S, l: LocalTemporary) => for {
         p <- l.path
         f <- l.fileWithContent(s.value)
         n <- f.copy(p)
         e <- f.exists
         r <- n.readOrFail
       } yield e -> r ==== true -> s.value)
     }

    ${ prop((s: S, l: LocalTemporary) => for {
         p <- l.path
         d <- l.directory
         f <- l.fileWithContent(s.value)
         n <- f.copyTo(d)
         a <- f.exists
         r <- n.readOrFail
       } yield a -> r ==== true -> s.value)
     }

    ${ prop((s: S, l: LocalTemporary) => for {
         p <- l.path
         d <- l.directory
         f <- p.write(s.value)
         n <- f.copyTo(d)
       } yield n.toLocalPath ==== (d.toLocalPath | p.basename.get))
     }

    ${ prop((s: S, l: LocalTemporary) => for {
         p <- l.path
         _ <- p.touch
         f <- l.fileWithContent(s.value)
         n <- f.copyWithMode(p, TargetMode.Overwrite)
         r <- n.readOrFail
       } yield r ==== s.value)
     }

    ${ prop((s: S, l: LocalTemporary) => (for {
         p <- l.path
         _ <- p.touch
         f <- l.fileWithContent(s.value)
         n <- f.copyWithMode(p, TargetMode.Fail)
       } yield ()) must beFail)
     }

    Handle copy failures

      ${ prop((s: S, l: LocalTemporary) => (for {
           p <- l.path
           _ <- p.touch
           f <- l.fileWithContent(s.value)
           _ <- f.delete
           _ <- f.copy(p)
         } yield ()) must beFail)
       }

      ${ prop((s: S, l: LocalTemporary) => (for {
           p <- l.path
           _ <- p.touch
           f <- l.fileWithContent(s.value)
           _ <- f.copy(p)
        } yield ()) must beFail)
       }

"""

  def pairForCodec(i: DistinctPair[S], c: Codec): Boolean =
    validForCodec(i.first, c) && validForCodec(i.second, c)

  def validForCodec(s: S, c: Codec): Boolean =
    new String(s.value.getBytes(c.name), c.name) == s.value

  def noNewLines(s: S): String =
    s.value.replaceAll("\\s", "")

  implicit val BooleanMonoid: Monoid[Boolean] =
    scalaz.std.anyVal.booleanInstance.conjunction
}
