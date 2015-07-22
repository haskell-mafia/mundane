package com.ambiata.mundane.io

import java.io.File
import java.net.URI

import org.scalacheck._, Arbitrary._, Gen._
import org.specs2.{ScalaCheck, Specification}
import org.specs2.matcher.Matcher

class FilePathSpec extends Specification with ScalaCheck { def is = s2"""

 Paths are of 2 sorts:

  - file paths
  - directory paths

 The essential difference is only director paths can be appended with a path name and become a FilePath.

 They also map to the notion of file and directory on a filesystem (but not on the Java notion of File which can represent both)

 DirPaths
 ========

 A DirPath can be created from
   a String
   ${ DirPath.unsafe("hello/world").path === "hello/world"  }
   a File
   ${ DirPath.unsafe(new File("/hello/world")).path === "/hello/world"  }
   a URI
   ${ DirPath.unsafe(new URI("hello/world")).path === "hello/world"  }
   ${ DirPath.unsafe(new URI("hdfs://100.100.1:9000/hello/world")).path === "/hello/world"  }
   ${ DirPath.unsafe(new URI("hdfs:/hello/world")).path === "/hello/world"  }
   ${ DirPath.unsafe(new URI("file:/hello/world")).path === "/hello/world"  }
   ${ DirPath.unsafe(new URI("s3://hello/world")).path === "/world"  }

 An absolute dir path can be built from
   a string starting with a /
   ${ DirPath.unsafe("/hello/world").isAbsolute }
   the DirPath.Root object
   ${ (DirPath.Root </> "world").isAbsolute }
   appending a DirPath to the Root
   ${ (DirPath.Root </> (DirPath.Empty </> "world")).isAbsolute }
   // this combination is accepted but should not be valid...
   ${ (DirPath.Root </> (DirPath.Root </> "world")).isAbsolute }

 A relative dir path can be built from
   a string not starting with a
   ${ DirPath.unsafe("hello/world").isRelative }
   the DirPath.Empty object
   ${ (DirPath.Empty </> "world").isRelative }
   a literal string
   ${ ("hello" </> "world").isRelative }

 Basic operations can be executed on a DirPath
   get the parent
   ${ DirPath.Root.parent must beNone }
   ${ DirPath("test").parent must beSome(DirPath.Empty) }
   ${ (DirPath.Root </> "test").parent must beSome(DirPath.Root) }
   ${ ("test" </> "hello" </> "world").parent must beSome("test" </> "hello") }

   get the basename
   ${ ("test" </> "hello" </> "world").basename === FileName.unsafe("world") }

   get the rootname
   ${ ("test" </> "hello" </> "world").rootname must_== DirPath.unsafe("test") }

   get the path as a string
   ${ DirPath.Root.path must_== "/" }
   ${ DirPath("test").path must_== "test" }
   ${ ("test" </> "hello" </> "world").path must_== "test/hello/world" }

   get the path as a string, with a last slash
   ${ DirPath.Root.dirPath must_== "/" }
   ${ DirPath("test").dirPath must_== "test/" }
   ${ ("test" </> "hello" </> "world").dirPath must_== "test/hello/world/" }

   get a portion of the path
   ${ prop((dir1: DirPath, dir2: DirPath) => (dir1 </> dir2).asAbsolute.relativeTo(dir1.asAbsolute) === dir2.asRelative) }
   ${ prop((dir1: DirPath, dir2: DirPath) => (dir1 </> "this" </> dir2).relativeTo(dir1 </> "that") === (dir1 </> "this" </> dir2)) }
   ${ prop((dir1: DirPath, dir2: DirPath) => (dir1 </> dir2).asRelative.relativeTo(dir1) === (dir1 </> dir2).asRelative) }

   ${ prop((dir1: DirPath, dir2: DirPath) =>
        dir1.asAbsolute.commonPrefix(dir2.asAbsolute) </> dir1.asAbsolute.removeCommonPrefix(dir2.asAbsolute) ===
        dir1.asAbsolute) }
   ${ prop((dir1: DirPath, dir2: DirPath) => dir1.removeCommonPrefix(dir2).isRelative === dir1.isRelative) }

   ${ ("test" </> "hello" </> "world").fromRoot === "hello" </> "world" }
   ${ ("test" </> "hello" </> "world").names === List("test", "hello", "world").map(FileName.unsafe) }
   ${ ("test" </> "hello" </> "world" </> "hi").up(2) === "test" </> "hello" }
   ${ ("test" </> "hello" </> "world" </> "hi").down(2) === "world" </> "hi" }

   filter hidden directories from a list
   ${ List("hello" </> ".world", "hello" </> "world", "hello" </> "_STATS").filterHidden === List("hello" </> "world") }

 FilePaths
 ========

 A FilePath can be created from
   a String
   ${ FilePath.unsafe("hello/world").path === "hello/world"  }
   a File
   ${ FilePath.unsafe(new File("hello/world")).path === "hello/world" }
   a URI
   ${ FilePath.unsafe(new URI("hello/world")).path === "hello/world"  }

   get the path as a string
   ${ FilePath("test").path must_== "test" }
   ${ ("test" </> "hello" <|> "world").path must_== "test/hello/world" }
   ${ ("test" </> "hello" </> DirPath.Empty.toFilePath).path must_== "test/hello" }

   filter hidden files from a list
   ${ List("hello" <|> ".world", "hello" <|> "world", "hello" <|> "_SUCCESS").filterHidden === List("hello" <|> "world") }

"""
  def beRelative: Matcher[DirPath] = { dirPath: DirPath =>
    (dirPath.isRelative, s"${dirPath} is not relative")
  }

  def beAbsolute: Matcher[DirPath] = { dirPath: DirPath =>
    (dirPath.isAbsolute, s"${dirPath} is not absolute")
  }

  def beRelative(implicit p1: ImplicitParam1): Matcher[FilePath] = { filePath: FilePath =>
    (filePath.isRelative, s"${filePath} is not relative")
  }

  def beAbsolute(implicit p1: ImplicitParam1): Matcher[FilePath] = { filePath: FilePath =>
    (filePath.isAbsolute, s"${filePath} is not absolute")
  }

  implicit def ArbitraryFilePath: Arbitrary[FilePath] =
    Arbitrary(arbitrary[DirPath].map(_.toFilePath))

  implicit def ArbitraryDirPath: Arbitrary[DirPath] = Arbitrary {
    for {
      n        <- choose(0, 5)
      names    <- listOfN(n, arbitrary[FileName])
      absolute <- arbitrary[Boolean]
    } yield DirPath(names.toVector, isAbsolute = absolute)
  }

  implicit def ArbitraryFileName: Arbitrary[FileName] = Arbitrary {
    nonEmptyListOf(alphaChar).map(_.mkString).suchThat(_.forall(_.isLetter)).map(FileName.unsafe)
  }
}
