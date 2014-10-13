package com.ambiata.mundane.io

import java.io.File
import java.net.URI

import org.specs2.Specification
import org.specs2.matcher.Matcher

class FilePathMiniSpec extends Specification { def is = s2"""

   a String
   ${ DirPath.fromString("hello/world").map(_.path) === Some("hello/world")  }

"""

 }
class FilePathSpec extends Specification { def is = s2"""

 Paths are of 2 sorts:

  - file paths
  - directory paths

 The essential difference is only director paths can be appended with a path name and become a FilePath.

 They also map to the notion of file and directory on a filesystem (but not on the Java notion of File which can represent both)

 DirPaths
 ========

 A DirPath can be created from
   a String
   ${ DirPath.fromString("hello/world").map(_.path) === Some("hello/world")  }
   a File
   ${ DirPath.fromFile(new File("/hello/world")).path === "/hello/world"  }
   a URI
   ${ DirPath.fromURI(new URI("hello/world")).map(_.path) === Some("hello/world")  }
   ${ DirPath.fromURI(new URI("hdfs://100.100.1:9000/hello/world")).map(_.path) === Some("/hello/world") }
   ${ DirPath.fromURI(new URI("hdfs:/hello/world")).map(_.path) === Some("/hello/world")  }
   ${ DirPath.fromURI(new URI("file:/hello/world")).map(_.path) === Some("/hello/world")  }
   ${ DirPath.fromURI(new URI("s3://hello/world")).map(_.path) === Some("/world")  }

 An absolute dir path can be built from
   a string starting with a /
   ${ DirPath.fromString("/hello/world").exists(_.isAbsolute) }
   the DirPath.Root object
   ${ (DirPath.Root </> "world").isAbsolute }
   appending a DirPath to the Root
   ${ (DirPath.Root </> (DirPath.Relative </> "world")).isAbsolute }
   // this combination is accepted but should not be valid...
   ${ (DirPath.Root </> (DirPath.Root </> "world")).isAbsolute }

 A relative dir path can be built from
   a string not starting with a
   ${ DirPath.fromString("hello/world").exists(_.isRelative) }
   the DirPath.Relative object
   ${ (DirPath.Relative </> "world").isRelative }
   a literal string
   ${ ("hello" </> "world").isRelative }

 Basic operations can be executed on a DirPath
   get the parent
   ${ DirPath.Root.parent must beNone }
   ${ DirPath.unsafe("test").parent must beSome(DirPath.Relative) }
   ${ (DirPath.Root </> "test").parent must beSome(DirPath.Root) }
   ${ ("test" </> "hello" </> "world").parent must beSome("test" </> "hello") }

   get the basename
   ${ ("test" </> "hello" </> "world").basename === Some(FileName.unsafe("world")) }

   get the path as a string
   ${ DirPath.Root.path must_== "/" }
   ${ DirPath.unsafe("test").path must_== "test" }
   ${ ("test" </> "hello" </> "world").path must_== "test/hello/world" }

   get the path as a string, with a last slash
   ${ DirPath.Root.dirPath must_== "/" }
   ${ DirPath.unsafe("test").dirPath must_== "test/" }
   ${ ("test" </> "hello" </> "world").dirPath must_== "test/hello/world/" }

   get a portion of the path
   ${ ("test" </> "hello" </> "world" </> "eric").relativeTo("test" </> "hello")  === Some("world" </> "eric") }
   ${ ("test" </> "hello" </> "world" </> "eric").relativeTo("test" </> "hello")  must beSome(beRelative) }
   ${ ("test" </> "hello" </> "world" </> "eric").relativeTo("other" </> "hello") must beNone }
   ${ ("test" </> "hello" </> "world").names === List("test", "hello", "world").map(FileName.unsafe) }

   filter hidden directories from a list
   ${ List("hello" </> ".world", "hello" </> "world").filterHidden === List("hello" </> "world") }

 FilePaths
 ========

 A FilePath can be created from
   a String
   ${ FilePath.unsafe("hello/world").path === "hello/world"  }
   a File
   ${ FilePath.fromFile(new File("hello/world")).path === "hello/world" }
   a URI
   ${ FilePath.fromURI(new URI("hello/world")).map(_.path) === Some("hello/world")  }

   get the path as a string
   ${ FilePath.unsafe("test").path must_== "test" }
   ${ (DirPath.Relative </> "test" </> "hello" </> "world").path must_== "test/hello/world" }
   ${ (DirPath.Relative </> "test" </> "hello" </> DirPath.Relative).path must_== "test/hello" }

   filter hidden files from a list
   ${ List("hello" </> ".world", "hello" </> "world", "hello" </> "_SUCCESS").filterHidden === List("hello" </> "world") }

"""
  def beRelative: Matcher[DirPath] = { dirPath: DirPath =>
    (dirPath.isRelative, s"${dirPath} is not relative")
  }

  def beAbsolute: Matcher[DirPath] = { dirPath: DirPath =>
    (dirPath.isAbsolute, s"${dirPath} is not absolute")
  }
/*
  def beRelative(implicit p1: ImplicitParam1): Matcher[FilePath] = { filePath: FilePath =>
    (filePath.isRelative, s"${filePath} is not relative")
  }

  def beAbsolute(implicit p1: ImplicitParam1): Matcher[FilePath] = { filePath: FilePath =>
    (filePath.isAbsolute, s"${filePath} is not absolute")
  }
*/
}
