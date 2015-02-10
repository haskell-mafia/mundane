package com.ambiata.mundane.io

import com.ambiata.disorder._
import com.ambiata.mundane.control.{Result => RR, _}
import com.ambiata.mundane.io.Arbitraries._
import com.ambiata.mundane.io.LocalPath._
import com.ambiata.mundane.path._
import com.ambiata.mundane.path.Arbitraries._
import com.ambiata.mundane.testing.RIOMatcher._
import MemoryConversions._
import java.io.File
import java.net.URI

import org.specs2._
import org.specs2.matcher.Matcher
import org.specs2.matcher.MatchResult
import org.specs2.matcher.DisjunctionMatchers

import scala.io.Codec
import scalaz.{Failure => _, _}, Scalaz._, effect.Effect._

class LocalPathSpec extends Specification with ScalaCheck with DisjunctionMatchers { def is = s2"""

 Paths are of 2 sorts:

  - file paths
  - directory paths

 The essential difference is only directory paths can be appended with a path name and become a LocalFile.

 They also map to the notion of file and directory on a filesystem (but not on the Java notion of File which can represent both)


 Pure operations
 ===============

  LocalPath operations should have the symenatics as Path operations

    ${ prop((l: Path, p: Path) => (LocalPath(l) / p).path ==== l / p) }

    ${ prop((l: Path, p: Path) => (LocalPath(l).join(p)).path ==== l.join(p)) }

    ${ prop((l: Path, p: Component) => (LocalPath(l) | p).path ==== (l | p)) }

    ${ prop((l: Path, p: Component) => (LocalPath(l).extend(p)).path ==== l.extend(p)) }

    ${ prop((l: Path, p: S) => (LocalPath(l) /- p.value).path ==== l /- p.value) }

    ${ prop((l: Path, p: Component) => (LocalPath(l) | p).rebaseTo(LocalPath(l)).map(_.path) ==== (l | p).rebaseTo(l)) }

    ${ prop((l: Path) => LocalPath(l).dirname.path ==== l.dirname) }

    ${ prop((l: Path) => LocalPath(l).basename ==== l.basename) }

  'LocalPath.fromFile' is symmetric with Path#toFile:

    ${ prop((p: Path) => LocalPath.fromFile(p.toFile).map(_.path) must beOkValue(p)) }

 LocalPath IO
 ============

  LocalPath should be able to determine files, directories and handle failure cases

    ${ LocalTemporary.random.path.flatMap(path => path.touch >> path.determine) must beOkLike(_ must beFile) }

    ${ LocalTemporary.random.path.flatMap(path => path.mkdirs >> path.determine) must beOkLike(_ must beDirectory) }

    ${ LocalTemporary.random.path.flatMap(path => path.determine) must beOkLike(_ must beNone) }

    ${ LocalPath(Path("")).determine must beOkLike(_ must beNone) }

  LocalPath can determine a file and handle failure cases

    ${ LocalTemporary.random.path.flatMap(path => path.touch >> path.determineFile) must beOk }

    ${ LocalTemporary.random.path.flatMap(path => path.mkdirs >> path.determineFile) must beFailWithMessage("Not a valid file") }

    ${ LocalTemporary.random.path.flatMap(path => path.determineFile) must beFailWithMessage("Not a valid File or Directory") }

  LocalPath can determine a directory and handle failure cases

    ${ LocalTemporary.random.path.flatMap(path => path.touch >> path.determineDirectory) must beFailWithMessage("Not a valid directory") }

    ${ LocalTemporary.random.path.flatMap(path => path.mkdirs >> path.determineDirectory) must beOk }

    ${ LocalTemporary.random.path.flatMap(path => path.determineDirectory) must beFailWithMessage("Not a valid File or Directory") }


  LocalPath should be able to perform these basic operations

    Check if a path exists

      ${ prop((l: LocalTemporary) => l.path.flatMap(p => p.touch >> p.exists) must beOkValue(true)) }

      ${ prop((l: LocalTemporary) => l.path.flatMap(p => p.mkdirs >> p.exists) must beOkValue(true)) }

      ${ prop((l: LocalTemporary) => l.path.flatMap(_.exists) must beOkValue(false)) }

      ${ prop((l: LocalTemporary) => { var i = 0; l.path.flatMap(p => p.touch >> p.doesExist("", RIO.io({
           i = 1; i }))) must beOkValue(1) }) }

      ${ prop((l: LocalTemporary) => { var i = 0; l.path.flatMap(p => p.touch >>
           p.whenExists(RIO.io({ i = 1; i }).void)).as(i) must beOkValue(1) }) }

      ${ prop((l: LocalTemporary) => { var i = 0; l.path.flatMap(_.doesNotExist("",
           RIO.io({ i = 1; i }).void)).as(i) must beOkValue(1) }) }

    Delete a path

      ${ prop((l: LocalTemporary) => for {
           p <- l.path
           _ <- p.touch
           b <- p.exists
           _ <- p.delete
           r <- p.exists
         } yield b -> r ==== true -> false)
       }

      ${ prop((l: LocalTemporary) => for {
           p <- l.path
           _ <- p.mkdirs
           b <- p.exists
           _ <- p.delete
           r <- p.exists
         } yield b -> r ==== true -> false)
       }

      ${ prop((l: LocalTemporary) => (for {
           p <- l.path
           _ <- p.delete
         } yield ()) must beFail)
       }

    Checksum a path

      ${ prop((s: S, l: LocalTemporary) => for {
           p <- l.path
           _ <- p.write(s.value)
           r <- p.checksum(MD5)
         } yield r ==== Checksum.string(s.value, MD5).some)
       }

      ${ prop((s: S, l: LocalTemporary) => (for {
           p <- l.path
           _ <- p.mkdirs
           r <- p.checksum(MD5)
         } yield r) must beFail)
       }

      ${ prop((s: S, l: LocalTemporary) => for {
           p <- l.path
           r <- p.checksum(MD5)
         } yield r ==== None)
       }

    Count the number of lines in a file

      ${ prop((s: List[Int], l: LocalTemporary) => for {
           p <- l.path
           _ <- p.writeLines(s.map(_.toString))
           r <- p.lineCount
         } yield r ==== s.length.some)
       }

      ${ prop((s: S, l: LocalTemporary) => for {
           p <- l.path
           r <- p.lineCount
         } yield r ==== None)
       }


  LocalPath should be able to read and write to a file. These operations should be symmetrical

    Write a string to a file and read it back

      ${ prop((s: S, l: LocalTemporary) => for {
           p <- l.path
           _ <- p.write(s.value)
           r <- p.readOrFail
         } yield r ==== s.value)
       }

      ${ prop((v: S, l: LocalTemporary) => for {
           p <- l.path
           _ <- p.write(v.value)
           r <- p.read
         } yield r ==== v.value.some)
       }

    Read and write a string with a specific encoding

      ${ prop((v: S, c: Codec, l: LocalTemporary) => validForCodec(v, c) ==> (for {
           p <- l.path
           _ <- p.writeWithEncoding(v.value, c)
           r <- p.readWithEncoding(c)
         } yield r ==== v.value.some))
       }

    Read and write a lines to a file

      ${ prop((v: List[S], l: LocalTemporary) => for {
           p <- l.path
           _ <- p.writeLines(v.map(noNewLines))
           r <- p.readLines
         } yield r ==== v.map(noNewLines).some)
       }

    Read and write lines with a specific encoding to a file

      ${ prop((v: List[S], c: Codec, l: LocalTemporary) => v.map(validForCodec(_, c)).suml ==> (for {
           p <- l.path
           _ <- p.writeLinesWithEncoding(v.map(noNewLines), c)
           r <- p.readLinesWithEncoding(c)
         } yield r ==== v.map(noNewLines).some))
       }

    Read a file using an InputStream

      ${ prop((s: S, l: LocalTemporary) => {
           var x: String = "";
           for {
             p <- l.path
             _ <- p.write(s.value)
             _ <- p.readUnsafe(in => for {
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
             _ <- p.writeLines(list.map(noNewLines))
             r <- p.doPerLine(s =>
               RIO.safe({ i += s; () }))
           } yield i.toList ==== list.map(noNewLines)
         })
       }

    Fold over each line read, keeping an accumulator

      ${ prop((list: List[S], l: LocalTemporary) => for {
           p <- l.path
           _ <- p.writeLines(list.map(noNewLines))
           r <- p.readPerLine(scala.collection.mutable.ListBuffer[String]())((s, b) => { b +=s; b})
         } yield r.toList ==== list.map(noNewLines))
       }

    Read lines with a specific encoding from a file

      ${ prop((list: List[S], c: Codec, l: LocalTemporary) => list.map(validForCodec(_, c)).suml ==> (for {
           p <- l.path
           _ <- p.writeLinesWithEncoding(list.map(noNewLines), c)
           r <- p.readPerLineWithEncoding(c, scala.collection.mutable.ListBuffer[String]())((s, b) => { b +=s; b})
         } yield r.toList ==== list.map(noNewLines)))
       }

    Read and write bytes to a file

      ${ prop((bs: Array[Byte], l: LocalTemporary) => for {
           p <- l.path
           _ <- p.writeBytes(bs)
           r <- p.readBytes
         } yield r.map(_.toList) ==== bs.toList.some)
       }

    Handle failure cases

      ${ prop((l: LocalTemporary) => l.path.flatMap(_.read) must beOkLike(_ must beNone)) }

      ${ prop((l: LocalTemporary) => l.path.flatMap(_.readOrFail) must beFail) }


   Write stream

     ${ prop((s: S, l: LocalTemporary) => for {
          p <- l.path
          a <- p.write(s.value)
          b <- l.path
          _ <- RIO.using(a.toInputStream)(in => b.writeStream(in))
          r <- b.readOrFail
        } yield r ==== s.value)
      }


  LocalPath should be able to touch files which will update the last modified time, but not affect the content.

    ${ prop((v: S, l: LocalTemporary) => for {
         p <- l.path
         _ <- p.write(v.value)
         _ <- p.touch
         r <- p.readOrFail
       } yield r ==== v.value)
     }

    ${ prop((l: LocalTemporary) => for {
         p <- l.path
         _ <- p.touch
         b <- RIO.safe(p.toFile.lastModified)
         _ <- RIO.safe(Thread.sleep(1100))
         _ <- p.touch
         a <- RIO.safe(p.toFile.lastModified)
       } yield b must be_<(a)).set(minTestsOk = 3)
     }

 LocalPath should be able to append different content to files

    ${ prop((d: DistinctPair[S], l: LocalTemporary) => for {
         p <- l.path
         _ <- p.write(d.first.value)
         _ <- p.append(d.second.value)
         r <- p.readOrFail
       } yield r ==== (d.first.value ++ d.second.value))
     }

    ${ prop((d: DistinctPair[S], c: Codec, l: LocalTemporary) =>
       (validForCodec(d.first, c) && validForCodec(d.second, c)) ==> (for {
         p <- l.path
         _ <- p.writeWithEncoding(d.first.value, c)
         _ <- p.appendWithEncoding(d.second.value, c)
         r <- p.readWithEncoding(c)
       } yield r ==== (d.first.value ++ d.second.value).some))
     }

    ${ prop((i: List[S], s: List[S], l: LocalTemporary) => for {
         p <- l.path
         _ <- p.writeLines(i.map(noNewLines))
         _ <- p.appendLines(s.map(noNewLines))
         r <- p.readLines
       } yield r ==== (i ++ s).map(noNewLines).some)
     }

    ${ prop((i: List[S], s: List[S], c: Codec, l: LocalTemporary) =>
       (i.map(validForCodec(_, c)) ++ s.map(validForCodec(_, c))).suml ==> (for {
         p <- l.path
         _ <- p.writeLinesWithEncoding(i.map(noNewLines), c)
         _ <- p.appendLinesWithEncoding(s.map(noNewLines), c)
         r <- p.readLinesWithEncoding(c)
       } yield r ==== (i ++ s).map(noNewLines).some))
     }

    ${ prop((i: Array[Byte], s: Array[Byte], l: LocalTemporary) => for {
         p <- l.path
         _ <- p.writeBytes(i)
         _ <- p.appendBytes(s)
         r <- p.readBytes
       } yield r.map(_.toList) ==== (i ++ s).toList.some)
     }

 LocalPath should be able to overwrite different content to files

    ${ prop((d: DistinctPair[S], l: LocalTemporary) => for {
         p <- l.path
         _ <- p.write(d.first.value)
         _ <- p.overwrite(d.second.value)
         r <- p.readOrFail
       } yield r ==== d.second.value)
     }

    ${ prop((d: DistinctPair[S], c: Codec, l: LocalTemporary) =>
       (validForCodec(d.first, c) && validForCodec(d.second, c)) ==> (for {
         p <- l.path
         _ <- p.writeWithEncoding(d.first.value, c)
         _ <- p.overwriteWithEncoding(d.second.value, c)
         r <- p.readWithEncoding(c)
       } yield r ==== d.second.value.some))
     }

    ${ prop((i: List[S], s: List[S], l: LocalTemporary) => for {
         p <- l.path
         _ <- p.writeLines(i.map(noNewLines))
         _ <- p.overwriteLines(s.map(noNewLines))
         r <- p.readLines
       } yield r ==== s.map(noNewLines).some)
     }

    ${ prop((i: List[S], s: List[S], c: Codec, l: LocalTemporary) =>
       (i.map(validForCodec(_, c)) ++ s.map(validForCodec(_, c))).suml ==> (for {
         p <- l.path
         _ <- p.writeLinesWithEncoding(i.map(noNewLines), c)
         _ <- p.overwriteLinesWithEncoding(s.map(noNewLines), c)
         r <- p.readLinesWithEncoding(c)
       } yield r ==== s.map(noNewLines).some))
     }

    ${ prop((i: Array[Byte], s: Array[Byte], l: LocalTemporary) => for {
         p <- l.path
         _ <- p.writeBytes(i)
         _ <- p.overwriteBytes(s)
         r <- p.readBytes
       } yield r.map(_.toList) ==== s.toList.some)
     }

   Overwrite stream

     ${ prop((s: DistinctPair[S], l: LocalTemporary) => for {
          p <- l.path
          a <- p.write(s.first.value)
          b <- l.fileWithContent(s.second.value)
          _ <- RIO.using(a.toInputStream)(in => b.toLocalPath.overwriteStream(in))
          r <- b.readOrFail
        } yield r ==== s.first.value)
      }

 LocalPath should be able to write with different modes

    Can write to a file with different mode's.

      ${ prop((s: DistinctPair[S], l: LocalTemporary) => for {
           p <- l.path
           _ <- p.write(s.first.value)
           _ <- p.writeWithMode(s.second.value, WriteMode.Append)
           r <- p.readOrFail
         } yield r ==== (s.first.value ++ s.second.value))
       }

      ${ prop((s: DistinctPair[S], l: LocalTemporary) => for {
           p <- l.path
           _ <- p.write(s.first.value)
           _ <- p.writeWithMode(s.second.value, WriteMode.Overwrite)
           r <- p.readOrFail
         } yield r ==== s.second.value)
       }

      ${ prop((s: DistinctPair[S], l: LocalTemporary) => (for {
           p <- l.path
           _ <- p.write(s.first.value)
           _ <- p.writeWithMode(s.second.value, WriteMode.Fail)
         } yield ()) must beFail)
       }

      ${ prop((s: S, l: LocalTemporary, mode: WriteMode) => (for {
           p <- l.path
           _ <- p.mkdirs
           _ <- p.writeWithMode(s.value, mode)
         } yield ()) must beFail)
       }

    Can write to files with different modes using different encodings

      ${ prop((s: DistinctPair[S], c: Codec, l: LocalTemporary) => pairForCodec(s, c) ==> (for {
           p <- l.path
           _ <- p.writeWithEncoding(s.first.value, c)
           _ <- p.writeWithEncodingMode(s.second.value, c, WriteMode.Append)
           r <- p.readWithEncoding(c)
         } yield r ==== (s.first.value ++ s.second.value).some))
       }

      ${ prop((s: DistinctPair[S], c: Codec, l: LocalTemporary) => pairForCodec(s, c) ==> (for {
           p <- l.path
           _ <- p.writeWithEncoding(s.first.value, c)
           _ <- p.writeWithEncodingMode(s.second.value, c, WriteMode.Overwrite)
           r <- p.readWithEncoding(c)
         } yield r ==== s.second.value.some))
       }

      ${ prop((s: DistinctPair[S], c: Codec, l: LocalTemporary) => pairForCodec(s, c) ==> { (for {
           p <- l.path
           _ <- p.writeWithEncoding(s.first.value, c)
           _ <- p.writeWithEncodingMode(s.second.value, c, WriteMode.Fail)
         } yield ()) must beFail })
       }

      ${ prop((s: S, c: Codec, l: LocalTemporary, mode: WriteMode) => validForCodec(s, c) ==> { (for {
           p <- l.path
           _ <- p.mkdirs
           _ <- p.writeWithEncodingMode(s.value, c, mode)
         } yield ()) must beFail })
       }

    Can write lines to a file with different mode's.

      ${ prop((a: List[S], b: List[S], l: LocalTemporary) => for {
           p <- l.path
           _ <- p.writeLines(a.map(noNewLines))
           _ <- p.writeLinesWithMode(b.map(noNewLines), WriteMode.Append)
           r <- p.readLines
         } yield r ==== (a ++ b).map(noNewLines).some)
       }

      ${ prop((a: List[S], b: List[S], l: LocalTemporary) => for {
           p <- l.path
           _ <- p.writeLines(a.map(noNewLines))
           _ <- p.writeLinesWithMode(b.map(noNewLines), WriteMode.Overwrite)
           r <- p.readLines
         } yield r ==== (b.map(noNewLines).some))
       }

      ${ prop((a: List[S], b: List[S], l: LocalTemporary) => (for {
           p <- l.path
           _ <- p.writeLines(a.map(noNewLines))
           _ <- p.writeLinesWithMode(b.map(noNewLines), WriteMode.Fail)
         } yield ()) must beFail)
       }

      ${ prop(( b: List[S], l: LocalTemporary, mode: WriteMode) => (for {
           p <- l.path
           _ <- p.mkdirs
           _ <- p.writeLinesWithMode(b.map(noNewLines), mode)
         } yield ()) must beFail)
       }

    Can write lines with different Codec's and Mode's

      ${ prop((a: List[S], b: List[S], c: Codec, l: LocalTemporary) =>
         (a ++ b).map(validForCodec(_, c)).suml ==> (for {
           p <- l.path
           _ <- p.writeLinesWithEncoding(a.map(noNewLines), c)
           _ <- p.writeLinesWithEncodingMode(b.map(noNewLines), c, WriteMode.Append)
           r <- p.readLinesWithEncoding(c)
         } yield r ==== (a ++ b).map(noNewLines).some))
       }

      ${ prop((a: List[S], b: List[S], c: Codec, l: LocalTemporary) =>
         (a ++ b).map(validForCodec(_, c)).suml ==> (for {
           p <- l.path
           _ <- p.writeLinesWithEncoding(a.map(noNewLines), c)
           _ <- p.writeLinesWithEncodingMode(b.map(noNewLines), c, WriteMode.Overwrite)
           r <- p.readLinesWithEncoding(c)
         } yield r ==== (b.map(noNewLines).some)))
       }

      ${ prop((a: List[S], b: List[S], c: Codec, l: LocalTemporary) =>
         (a ++ b).map(validForCodec(_, c)).suml ==> { (for {
           p <- l.path
           _ <- p.writeLinesWithEncoding(a.map(noNewLines), c)
           _ <- p.writeLinesWithEncodingMode(b.map(noNewLines), c, WriteMode.Fail)
         } yield ()) must beFail })
       }

      ${ prop((b: List[S], c: Codec, l: LocalTemporary, mode: WriteMode) =>
         b.map(validForCodec(_, c)).suml ==> { (for {
           p <- l.path
           _ <- p.mkdirs
           _ <- p.writeLinesWithEncodingMode(b.map(noNewLines), c, mode)
         } yield ()) must beFail })
       }

    Can write bytes with different Mode's

      ${ prop((a: Array[Byte], b: Array[Byte], l: LocalTemporary) => for {
           p <- l.path
           _ <- p.writeBytes(a)
           _ <- p.writeBytesWithMode(b, WriteMode.Append)
           r <- p.readBytes
         } yield r.map(_.toList) ==== (a ++ b).toList.some)
       }

      ${ prop((a: Array[Byte], b: Array[Byte], l: LocalTemporary) => for {
           p <- l.path
           _ <- p.writeBytes(a)
           _ <- p.writeBytesWithMode(b, WriteMode.Overwrite)
           r <- p.readBytes
         } yield r.map(_.toList) ==== b.toList.some)
       }

      ${ prop((a: Array[Byte], l: LocalTemporary) => (for {
           p <- l.path
           _ <- p.writeBytes(a)
           _ <- p.writeBytesWithMode(a, WriteMode.Fail)
         } yield ()) must beFail)
       }

      ${ prop((a: Array[Byte], l: LocalTemporary, mode: WriteMode) => (for {
           p <- l.path
           _ <- p.mkdirs
           _ <- p.writeBytesWithMode(a, mode)
         } yield ()) must beFail)
       }

 LocalPath should be able to list files/directories/paths at a single level

   List a single file

     ${ prop((l: LocalTemporary) => for {
          p <- l.path
          f <- p.touch
          r <- p.listFiles
        } yield r ==== List(f))
      }

   'listFiles' is consistent with 'determineFile'

     ${ prop((l: LocalTemporary) => (for {
          p <- l.path
          _ <- p.touch
          r <- p.listFiles
        } yield r.traverse(_.toLocalPath.determineFile)) must beOk)
      }

   List multiple files

     ${ prop((l: LocalTemporary, a: Component, b: Component, c: Component) =>
          (a.name != b.name && a.name != c.name) ==> (for {
            p <- l.path
            _ <- (p | a).touch
            _ <- (p | b).touch
            _ <- (p | c).mkdirs
            r <- bases(p.listFiles)
          } yield r.sorted ==== List(a, b).sorted))
      }

   List a directory

     ${ prop((v: DistinctPair[Component], l: LocalTemporary) => for {
          p <- l.path
          _ <- (p | v.first).mkdirs
          _ <- (p | v.second).touch
          r <- p.listDirectories.map(_.map(_.path.basename))
        } yield r ==== List(v.first.some))
      }

   List multiple paths

     ${ prop((v: DistinctPair[Component], l: LocalTemporary) => for {
          p <- l.path
          _ <- (p | v.first | v.second).touch
          _ <- (p | v.second).touch
          r <- p.listPaths.map(_.map(_.basename))
        } yield r.sorted ==== List(v.first.some, v.second.some).sorted)
      }

 LocalPath should be able to list files/directories/paths recursively

  List files

   ${ prop((d: DistinctPair[Component], l: LocalTemporary) => for {
        p <- l.path
        v =  d.first
        _ <- List(p | v | v | v | v, p | v | d.second, p | v | v | d.second).traverse(_.touch)
        r <- p.listFilesRecursively.map(_.map(_.path.rebaseTo(p.path)))
      } yield r.sorted ==== List((Relative | v | v | v | v).some, (Relative | v | d.second).some,
         (Relative | v | v | d.second).some).sorted)
     }

  List directories

   ${ prop((d: DistinctPair[Component], local: LocalTemporary) => for {
        p <- local.path
        x = d.first
        a = p | x | x | x
        b = p | x | x
        c = p | x
        _ <- List(a | x, b | d.second, c | d.second).traverse(_.touch)
        r <- p.listDirectoriesRecursively.map(_.map(_.toLocalPath))
      } yield r.sorted ==== List(a, b, c).sorted) }

  List paths

    ${ prop((v: DistinctPair[Component], local: LocalTemporary) => for {
         p <- local.path
         _ <- (p | v.first | v.second).touch
         _ <- (p | v.first | v.first | v.second).touch
         _ <- (p | v.second).touch
         r <- p.listPathsRecursively.map(_.map(_.path.rebaseTo(p.path)))
       } yield r.sorted ==== List(Path(v.first.name), Path(v.second.name), Path(v.first.name) | v.second,
          Path(v.first.name) | v.first, Path(v.first.name) | v.first | v.second).map(_.some).sorted)
     }

 LocalPath should be able to calculate the size of files/directories/paths

   Size of a single file

     ${ prop((v: S, l: LocalTemporary) => for {
          p <- l.path
          _ <- p.write(v.value)
          r <- p.size
        } yield r ==== v.value.getBytes.length.bytes)
      }

   Size of a directory

     ${ prop((f: DistinctPair[Component], v: DistinctPair[S], l: LocalTemporary) => for {
          p <- l.path
          _ <- (p | f.first).write(v.first.value)
          _ <- (p | f.second).write(v.second.value)
          r <- p.size
        } yield r ==== (v.first.value.getBytes.length + v.second.value.getBytes.length).bytes)
      }

 LocalPath should be able to move files/directories/paths

   Move a single file to a path

     ${ prop((l: LocalTemporary) => for {
          p <- l.path
          d <- l.path
          _ <- p.touch
          _ <- p.move(d)
          b <- p.exists
          a <- d.exists
        } yield b -> a ==== false -> true)
      }

   Move a single file to a directory

     ${ prop((v: Component, l: LocalTemporary) => for {
          p <- l.path
          d <- l.directory
          _ <- (p | v).touch
          _ <- (p | v).move(d.toLocalPath)
          b <- (p | v).exists
          a <- (d.toLocalPath | v).exists
        } yield b -> a ==== false -> true)
      }

   Move a single file to a file that exists should fail

     ${ prop((v: Component, l: LocalTemporary) => (for {
          p <- l.path
          d <- l.path
          _ <- (p | v).touch
          _ <- d.touch
          _ <- p.move(d)
        } yield ()) must beFail)
      }

   Move a directory to a path

     ${ prop((l: LocalTemporary) => for {
          p <- l.path
          d <- l.path
          _ <- p.mkdirs
          _ <- p.move(d)
          b <- p.exists
          a <- d.exists
        } yield b -> a ==== false -> true)
      }

   Move a directory to a directory

     ${ prop((v: DistinctPair[Component], l: LocalTemporary) => for {
          p <- l.path
          d <- l.path
          _ <- (p | v.first | v.second).touch
          _ <- d.mkdirs
          _ <- (p | v.first).move(d)
          b <- (p | v.first).exists
          a <- (d | v.first | v.second).exists
        } yield b -> a ==== false -> true)
      }

   Move a directory to a file that exists should fail

     ${ prop((v: Component, l: LocalTemporary) => (for {
          p <- l.path
          d <- l.path
          _ <- p.mkdirs
          _ <- d.touch
          _ <- p.move(d)
        } yield ()) must beFail)
      }

 LocalPath should be able to copy files/directories/paths

   Copy a single file to a path

     ${ prop((l: LocalTemporary) => for {
          p <- l.path
          d <- l.path
          _ <- p.touch
          _ <- p.copy(d)
          b <- p.exists
          a <- d.exists
        } yield b -> a ==== true -> true)
      }

   Copy a single file to a directory

     ${ prop((v: Component, l: LocalTemporary) => for {
          p <- l.path
          d <- l.directory
          _ <- (p | v).touch
          _ <- (p | v).copy(d.toLocalPath)
          b <- (p | v).exists
          a <- (d.toLocalPath | v).exists
        } yield b -> a ==== true -> true)
      }

   Copy a single file to a file that exists should file

     ${ prop((l: LocalTemporary) => (for {
          p <- l.path
          d <- l.path
          _ <- p.touch
          _ <- d.touch
          _ <- p.copy(d)
        } yield ()) must beFail)
      }

   Copy a directory to a path should be an unsupported operation

     ${ prop((l: LocalTemporary) => (for {
          p <- l.path
          d <- l.path
          _ <- p.mkdirs
          _ <- p.copy(d)
        } yield ()) must beFail)
      }

"""

  val beFile = beSome(be_-\/[LocalFile])
  val beDirectory = beSome(be_\/-[LocalDirectory])

  def bases(l: RIO[List[LocalFile]]): RIO[List[Component]] =
    l.map(_.map(_.path.basename).flatten)

  def noNewLines(s: S): String =
    s.value.replaceAll("\\s", "")

  def pairForCodec(i: DistinctPair[S], c: Codec): Boolean =
    validForCodec(i.first, c) && validForCodec(i.second, c)

  def validForCodec(s: S, c: Codec): Boolean =
    new String(s.value.getBytes(c.name), c.name) == s.value

  implicit val BooleanMonoid: Monoid[Boolean] =
    scalaz.std.anyVal.booleanInstance.conjunction
}
