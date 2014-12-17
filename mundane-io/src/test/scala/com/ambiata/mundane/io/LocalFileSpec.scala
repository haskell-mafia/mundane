package com.ambiata.mundane.io

import com.ambiata.mundane.path._
import java.io.File
import java.net.URI

import org.specs2.Specification
import org.specs2.matcher.Matcher

class LocalFileMiniSpec extends Specification { def is = s2"""

   a String
   ${ LocalDirectory.fromString("hello/world").map(_.path) === Some("hello/world")  }

"""

 }
class LocalFileSpec extends Specification { def is = s2"""

 Paths are of 2 sorts:

  - file paths
  - directory paths

 The essential difference is only director paths can be appended with a path name and become a LocalFile.

 They also map to the notion of file and directory on a filesystem (but not on the Java notion of File which can represent both)

 LocalDirectorys
 ========

 A LocalDirectory can be created from
   a String
   ${ LocalDirectory.fromString("hello/world").map(_.path) === Some("hello/world")  }
   a File
   ${ LocalDirectory.fromFile(new File("/hello/world")).path === "/hello/world"  }
   a URI
   ${ LocalDirectory.fromURI(new URI("hello/world")).map(_.path) === Some("hello/world")  }
   ${ LocalDirectory.fromURI(new URI("hdfs://100.100.1:9000/hello/world")).map(_.path) === Some("/hello/world") }
   ${ LocalDirectory.fromURI(new URI("hdfs:/hello/world")).map(_.path) === Some("/hello/world")  }
   ${ LocalDirectory.fromURI(new URI("file:/hello/world")).map(_.path) === Some("/hello/world")  }
   ${ LocalDirectory.fromURI(new URI("s3://hello/world")).map(_.path) === Some("/world")  }

 An absolute dir path can be built from
   a string starting with a /
   ${ LocalDirectory.fromString("/hello/world").exists(_.isAbsolute) }
   the LocalDirectory.Root object
   ${ (LocalDirectory.Root </ "world").isAbsolute }
   appending a LocalDirectory to the Root
   ${ (LocalDirectory.Root </> (LocalDirectory.Relative </ "world")).isAbsolute }
   // this combination is accepted but should not be valid...
   ${ (LocalDirectory.Root </> (LocalDirectory.Root </ "world")).isAbsolute }

 A relative dir path can be built from
   a string not starting with a
   ${ LocalDirectory.fromString("hello/world").exists(_.isRelative) }
   the LocalDirectory.Relative object
   ${ (LocalDirectory.Relative </ "world").isRelative }
   a literal string
   ${ ("hello" </ "world").isRelative }

 Basic operations can be executed on a LocalDirectory
   get the parent
   ${ LocalDirectory.Root.parent must beNone }
   ${ LocalDirectory.unsafe("test").parent must beSome(LocalDirectory.Relative) }
   ${ (LocalDirectory.Root </ "test").parent must beSome(LocalDirectory.Root) }
   ${ ("test" </ "hello" </ "world").parent must beSome("test" </ "hello") }

   get the basename
   ${ ("test" </ "hello" </ "world").basename === Some(FileName.unsafe("world")) }

   get the path as a string
   ${ LocalDirectory.Root.path must_== "/" }
   ${ LocalDirectory.unsafe("test").path must_== "test" }
   ${ ("test" </ "hello" </ "world").path must_== "test/hello/world" }

   get a portion of the path
   ${ ("test" </ "hello" </ "world" </ "eric").rebaseTo("test" </ "hello")  === Some("world" </ "eric") }
   ${ ("test" </ "hello" </ "world" </ "eric").rebaseTo("test" </ "hello")  must beSome(beRelative) }
   ${ ("test" </ "hello" </ "world" </ "eric").rebaseTo("other" </ "hello") must beNone }
   ${ ("test" </ "hello" </ "world").names === List("test", "hello", "world").map(FileName.unsafe) }

 LocalFiles
 ========

 A LocalFile can be created from
   a String
   ${ LocalFile.unsafe("hello/world").path === "hello/world"  }
   a File
   ${ LocalFile.fromFile(new File("hello/world")).path === "hello/world" }
   a URI
   ${ LocalFile.fromURI(new URI("hello/world")).map(_.path) === Some("hello/world")  }

   get the path as a string
   ${ LocalFile.unsafe("test").path must_== "test" }
   ${ (LocalDirectory.Relative </ "test" </ "hello" </ "world").path must_== "test/hello/world" }
   ${ (LocalDirectory.Relative </ "test" </ "hello" </> LocalDirectory.Relative).path must_== "test/hello" }

   filter hidden files from a list
   ${ List("hello" </ ".world", "hello" </ "world", "hello" </ "_SUCCESS").filterHidden === List("hello" </ "world") }

"""
  def beRelative: Matcher[LocalDirectory] = { dirPath: LocalDirectory =>
    (dirPath.isRelative, s"${dirPath} is not relative")
  }

  def beAbsolute: Matcher[LocalDirectory] = { dirPath: LocalDirectory =>
    (dirPath.isAbsolute, s"${dirPath} is not absolute")
  }
/*
  def beRelative(implicit p1: ImplicitParam1): Matcher[LocalFile] = { filePath: LocalFile =>
    (filePath.isRelative, s"${filePath} is not relative")
  }

  def beAbsolute(implicit p1: ImplicitParam1): Matcher[LocalFile] = { filePath: LocalFile =>
    (filePath.isAbsolute, s"${filePath} is not absolute")
  }
*/
}
