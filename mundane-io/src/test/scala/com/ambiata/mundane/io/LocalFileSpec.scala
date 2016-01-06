package com.ambiata.mundane.io

import com.ambiata.disorder._
import com.ambiata.mundane.path._
import com.ambiata.mundane.control._
import com.ambiata.mundane.io.Arbitraries._
import com.ambiata.mundane.testing.RIOMatcher._
import java.io.File
import java.net.URI

import org.specs2._
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

  'LocalFile.fromFile.toPath' is symmetric

    ${ prop((l: LocalTemporary) => for {
         p <- l.path
         f <- p.touch
         n <- LocalFile.fromFile(f.toFile)
       } yield p.path ==== n.toPath)
     }

  'LocalFile.toLocalPath' is symmetric with Path#determine

    ${ prop((l: LocalTemporary) => for { f <- l.file; r <- f.toLocalPath.determine } yield r ==== -\/[LocalFile](f).some) }


 LocalFiles
 ==========

 A LocalFile can be created from
   a String

     ${ LocalFile.unsafe("hello/world").path.path === "hello/world"  }

   a File

     ${ prop((l: LocalTemporary) => for {
          p <- l.path
          f <- p.touch
          n <- LocalFile.fromFile(f.toFile)
        } yield f ==== n)
      }

   a URI

     ${ prop((l: LocalTemporary) => for {
          p <- l.path
          f <- p.touch
          u = new File(f.path.path).toURI()
          n <- LocalFile.fromURI(u)
        } yield f.some ==== n)
      }

   get the path as a string

     ${ LocalFile.unsafe("test").path.path must_== "test" }

     ${ (LocalDirectory.Relative /- "test" /- "hello" /- "world").path must_== "test/hello/world" }

     ${ (LocalDirectory.Relative /- "test" /- "hello" / LocalDirectory.Relative).path must_== "test/hello" }

   filter hidden files from a list

     ${ LocalFile.filterHidden(List(LocalFile.unsafe("hello/.world"), LocalFile.unsafe("hello/world"),
          LocalFile.unsafe("hello/_SUCCESS"))) ==== List(LocalFile.unsafe("hello/world")) }

 A LocalFile can be ordered

   ${ List(LocalFile.unsafe("z"), LocalFile.unsafe("a")).sorted ==== List(LocalFile.unsafe("a"), LocalFile.unsafe("z")) }

  IO
  ==

  LocalFile should be able to perform these basic operations

    ${ LocalTemporary.random.file.flatMap(_.exists) must beOkValue(true) }

    ${ prop((l: LocalTemporary) => (for {
         p <- l.path
         f <- p.touch
         _ <- p.delete
         _ <- p.mkdirs
         _ <- f.exists
       } yield ()) must beFail)
     }

    ${ var i = 0; LocalTemporary.random.file.flatMap(_.whenExists(RIO.io({ i = 1; i }).void)).as(i) must beOkValue(1) }

    ${ var i = 0; LocalTemporary.random.file.flatMap(_.doesExist("", RIO.io({ i = 1; i }))) must beOkValue(1) }

    ${ LocalTemporary.random.file.flatMap(_.doesNotExist("", RIO.unit)) must beFail }

    ${ var i = 0; LocalFile.unsafe("test").doesNotExist("", RIO.io({ i = 1; i })) must beOkValue(1) }

    ${ LocalTemporary.random.file.flatMap(f => f.delete >> f.exists) must beOkValue(false) }

  LocalFile should be able to perform a checksum

    ${ prop((s: S, l: LocalTemporary) => for {
         f <- l.fileWithContent(s.value)
         r <- f.checksum(MD5)
       } yield r ==== Checksum.string(s.value, MD5).some)
     }

    ${ prop((s: S, l: LocalTemporary) => for {
         f <- l.fileWithContent(s.value)
         _ <- f.delete
         r <- f.checksum(MD5)
       } yield r ==== None)
     }

  LocalFile should be able to count the number of lines in a file

    ${ prop((s: List[Int], l: LocalTemporary) => for {
         p <- l.path
         f <- p.writeLines(s.map(_.toString))
         r <- f.lineCount
       } yield r ==== s.length.some)
     }

    ${ prop((l: LocalTemporary) => for {
         p <- l.path
         f <- p.touch
         r <- f.lineCount
       } yield r ==== 0.some)
     }

  LocalFile should be able to read different content from files using different methods

    Read a string from a file

      ${ prop((s: S, l: LocalTemporary) => for {
           f <- l.fileWithContent(s.value)
           r <- f.read
         } yield r ==== s.value.some)
       }

    Read a string with a specific encoding from a file

      ${ prop((c: Codec, s: S, l: LocalTemporary) => validForCodec(s, c) ==> (for {
           p <- l.path
           f <- p.writeWithEncoding(s.value, c)
           r <- f.readWithEncoding(c)
         } yield r ==== s.value.some))
       }

    Read lines from a file

      ${ prop((s: List[S], l: LocalTemporary) => for {
           p <- l.path
           f <- p.writeLines(s.map(noNewLines))
           r <- f.readLines
         } yield r ==== s.map(noNewLines).some)
       }

    Read a file using an InputStream

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

    Run a function for every line read in as a String

      ${ prop((list: List[S], l: LocalTemporary) => {
           var i = scala.collection.mutable.ListBuffer[String]()
           for {
             p <- l.path
             f <- p.writeLines(list.map(noNewLines))
             r <- f.doPerLine(s =>
               RIO.safe({ i += s; () }))
           } yield i.toList ==== list.map(noNewLines)
         })
       }

    Fold over each line read, keeping an accumulator

      ${ prop((list: List[S], l: LocalTemporary) => for {
           p <- l.path
           f <- p.writeLines(list.map(noNewLines))
           r <- f.readPerLine(scala.collection.mutable.ListBuffer[String]())((s, b) => { b +=s; b})
         } yield r.toList ==== list.map(noNewLines))
       }

    Read lines with a specific encoding from a file

      ${ prop((s: List[S], c: Codec, l: LocalTemporary) => s.map(validForCodec(_, c)).suml ==> (for {
           p <- l.path
           f <- p.writeLinesWithEncoding(s.map(noNewLines), c)
           r <- f.readLinesWithEncoding(c)
         } yield r ==== s.map(noNewLines).some))
       }

    Read bytes from a file

      ${ prop((bs: Array[Byte], l: LocalTemporary) => for {
           p <- l.path
           f <- p.writeBytes(bs)
           r <- f.readBytes
         } yield r.map(_.toList) ==== bs.toList.some)
       }

    Read string from a file or fail

      ${ prop((s: S, l: LocalTemporary) => for {
           f <- l.fileWithContent(s.value)
           r <- f.readOrFail
         } yield r ==== s.value)
       }

    Handle failure cases

     ${ prop((l: LocalTemporary) => l.file.flatMap(f => f.delete >> f.read) must beOkLike(_ must beNone)) }

     ${ prop((l: LocalTemporary) => l.file.flatMap(f => f.delete >> f.readOrFail) must beFail) }


  LocalFile should be able to append different content to files that exist

    Append a string to a file

      ${ prop((d: DistinctPair[S], l: LocalTemporary) => for {
           p <- l.path
           f <- p.write(d.first.value)
           _ <- f.append(d.second.value)
           r <- f.read
         } yield r ==== (d.first.value ++ d.second.value).some)
       }

    Append a string with a specific encoding to a file

      ${ prop((d: DistinctPair[S], c: Codec, l: LocalTemporary) =>
           (validForCodec(d.first, c) && validForCodec(d.second, c)) ==> (for {
             p <- l.path
             f <- p.writeWithEncoding(d.first.value, c)
             _ <- f.appendWithEncoding(d.second.value, c)
             r <- f.readWithEncoding(c)
           } yield r ==== (d.first.value ++ d.second.value).some))
       }

    Append list of strings to a file

      ${ prop((i: List[S], s: List[S], l: LocalTemporary) => for {
           p <- l.path
           f <- p.writeLines(i.map(noNewLines))
           _ <- f.appendLines(s.map(noNewLines))
           r <- f.readLines
         } yield r ==== (i ++ s).map(noNewLines).some)
       }

    Append list of strings with a specific encoding to a file

      ${ prop((i: List[S], s: List[S], c: Codec, l: LocalTemporary) =>
           (i.map(validForCodec(_, c)) ++ s.map(validForCodec(_, c))).suml ==> (for {
             p <- l.path
             f <- p.writeLinesWithEncoding(i.map(noNewLines), c)
             _ <- f.appendLinesWithEncoding(s.map(noNewLines), c)
             r <- f.readLinesWithEncoding(c)
           } yield r ==== (i ++ s).map(noNewLines).some))
       }

    Append bytes to a file

      ${ prop((i: Array[Byte], s: Array[Byte], l: LocalTemporary) => for {
           p <- l.path
           f <- p.writeBytes(i)
           _ <- f.appendBytes(s)
           r <- f.readBytes
         } yield r.map(_.toList) ==== (i ++ s).toList.some)
       }

  LocalFile should be able to write different content to files using different Encodings and Mode's.

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

      ${ prop((a: List[S], b: List[S], c: Codec, l: LocalTemporary) =>
         (a ++ b).map(validForCodec(_, c)).suml ==> (for {
           p <- l.path
           f <- p.writeLinesWithEncoding(a.map(noNewLines), c)
           _ <- f.writeLinesWithEncodingMode(b.map(noNewLines), c, WriteMode.Append)
           r <- f.readLinesWithEncoding(c)
         } yield r ==== (a ++ b).map(noNewLines).some))
       }

      ${ prop((a: List[S], b: List[S], c: Codec, l: LocalTemporary) =>
         (a ++ b).map(validForCodec(_, c)).suml ==> (for {
           p <- l.path
           f <- p.writeLinesWithEncoding(a.map(noNewLines), c)
           _ <- f.writeLinesWithEncodingMode(b.map(noNewLines), c, WriteMode.Overwrite)
           r <- f.readLinesWithEncoding(c)
         } yield r ==== (b.map(noNewLines).some)))
       }

      ${ prop((a: List[S], b: List[S], c: Codec, l: LocalTemporary) =>
         (a ++ b).map(validForCodec(_, c)).suml ==> { (for {
           p <- l.path
           f <- p.writeLinesWithEncoding(a.map(noNewLines), c)
           _ <- f.writeLinesWithEncodingMode(b.map(noNewLines), c, WriteMode.Fail)
         } yield ()) must beFail })
       }

    Can write bytes with different Mode's

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

    Overwrite a string in a file that exists and has content

      ${ prop((d: DistinctPair[S], l: LocalTemporary) => for {
           p <- l.path
           f <- p.write(d.first.value)
           _ <- f.overwrite(d.second.value)
           r <- f.read
         } yield r ==== d.second.value.some)
       }

    Overwrite a string in a file that doesn't exist

      ${ prop((d: DistinctPair[S], l: LocalTemporary) => for {
           p <- l.path
           f <- p.write(d.first.value)
           _ <- f.delete
           _ <- f.overwrite(d.second.value)
           r <- f.read
         } yield r ==== d.second.value.some)
       }

     Overwrite a string with a specific encoding in a file that exists and has content

       ${ prop((d: DistinctPair[S], c: Codec, l: LocalTemporary) =>
          (validForCodec(d.first, c) && validForCodec(d.second, c)) ==> (for {
            p <- l.path
            f <- p.writeWithEncoding(d.first.value, c)
            _ <- f.overwriteWithEncoding(d.second.value, c)
            r <- f.readWithEncoding(c)
          } yield r ==== d.second.value.some))
        }

     Overwrite a list of strings in a file that exists and has content

       ${ prop((i: S, s: List[S], l: LocalTemporary) => for {
            p <- l.path
            f <- p.write(i.value)
            _ <- f.overwriteLines(s.map(noNewLines))
            r <- f.readLines
          } yield r ==== s.map(noNewLines).some)
        }

     Overwrite a list of strings with a specific encoding in a file that exists and has content

       ${ prop((i: List[S], s: List[S], c: Codec, l: LocalTemporary) =>
          (i.map(validForCodec(_, c)) ++ s.map(validForCodec(_, c))).suml ==> (for {
            p <- l.path
            f <- p.writeLinesWithEncoding(i.map(noNewLines), c)
            _ <- f.overwriteLinesWithEncoding(s.map(noNewLines), c)
            r <- f.readLinesWithEncoding(c)
          } yield r ==== s.map(noNewLines).some))
        }

     Overwrite bytes in a file that exists and has content

       ${ prop((i: Array[Byte], s: Array[Byte], l: LocalTemporary) => for {
            p <- l.path
            f <- p.writeBytes(i)
            _ <- f.overwriteBytes(s)
            r <- f.readBytes
          } yield r.map(_.toList) ==== s.toList.some)
        }

  LocalFile should be able to move files

    ${ prop((s: S, l: LocalTemporary) => for {
         p <- l.path
         f <- l.fileWithContent(s.value)
         n <- f.move(p)
         e <- f.exists
         r <- n.readOrFail
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

    Copying a file should leave the original file intact

      ${ prop((s: S, l: LocalTemporary) => for {
           p <- l.path
           f <- l.fileWithContent(s.value)
           n <- f.copy(p)
           e <- f.exists
           r <- n.readOrFail
         } yield e -> r ==== true -> s.value)
       }

    Copying a file to a directory, should put the file inside the directory

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

    Copying a file with the 'Overwrite' mode should not fail when the target exists

      ${ prop((s: S, l: LocalTemporary) => for {
           p <- l.path
           _ <- p.touch
           f <- l.fileWithContent(s.value)
           n <- f.copyWithMode(p, TargetMode.Overwrite)
           r <- n.readOrFail
         } yield r ==== s.value)
       }

    Copying a file with the 'Fail' mode should fail when the target exists

      ${ prop((s: S, l: LocalTemporary) => (for {
           p <- l.path
           _ <- p.touch
           f <- l.fileWithContent(s.value)
           n <- f.copyWithMode(p, TargetMode.Fail)
         } yield ()) must beFail)
       }

    Copying a file should fail when the souce no longer exists

      ${ prop((s: S, l: LocalTemporary) => (for {
           p <- l.path
           _ <- p.touch
           f <- l.fileWithContent(s.value)
           _ <- f.delete
           _ <- f.copy(p)
         } yield ()) must beFail)
       }

    Copying a file should fail when the target exists

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
