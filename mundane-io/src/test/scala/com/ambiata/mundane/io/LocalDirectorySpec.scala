package com.ambiata.mundane.io

import com.ambiata.mundane.path._
import java.io.File
import java.net.URI

import org.specs2.Specification
import org.specs2.matcher.Matcher

class LocalDirectorySpec extends Specification { def is = s2"""

 Paths are of 2 sorts:

  - file paths
  - directory paths

 The essential difference is only directory paths can be appended with a path name and become a LocalFile.

 They also map to the notion of file and directory on a filesystem (but not on the Java notion of File which can represent both)

 a String
  ${ LocalDirectory.fromString("hello/world").map(_.path) === Some("hello/world")  }

 LocalDirectories
 ================

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
   ${ (LocalDirectory.Root </- "world").isAbsolute }
   appending a LocalDirectory to the Root
   ${ (LocalDirectory.Root </> (LocalDirectory.Relative </- "world")).isAbsolute }
   // this combination is accepted but should not be valid...
   ${ (LocalDirectory.Root </> (LocalDirectory.Root </- "world")).isAbsolute }

 A relative dir path can be built from
   a string not starting with a
   ${ LocalDirectory.fromString("hello/world").exists(_.isRelative) }
   the LocalDirectory.Relative object
   ${ (LocalDirectory.Relative </- "world").isRelative }
   a literal string
   ${ ("hello" </ "world").isRelative }

 Basic operations can be executed on a LocalDirectory
   get the parent
   ${ LocalDirectory.Root.parent must beNone }
   ${ LocalDirectory.unsafe("test").parent must beSome(LocalDirectory.Relative) }
   ${ (LocalDirectory.Root </- "test").parent must beSome(LocalDirectory.Root) }
   ${ ("test" </ "hello" </- "world").parent must beSome("test" </ "hello") }

   get the basename
   ${ ("test" </ "hello" </- "world").basename === Some(FileName.unsafe("world")) }

   get the path as a string
   ${ LocalDirectory.Root.path must_== "/" }
   ${ LocalDirectory.unsafe("test").path must_== "test" }
   ${ ("test" </ "hello" </- "world").path must_== "test/hello/world" }

   get a portion of the path
   ${ ("test" </ "hello" </- "world" </- "eric").rebaseTo("test" </ "hello")  === Some("world" </ "eric") }
   ${ ("test" </ "hello" </- "world" </- "eric").rebaseTo("test" </ "hello")  must beSome(beRelative) }
   ${ ("test" </ "hello" </- "world" </- "eric").rebaseTo("other" </ "hello") must beNone }
   ${ ("test" </ "hello" </- "world").names === List("test", "hello", "world").map(FileName.unsafe) }




"""
  def beRelative: Matcher[LocalDirectory] = { dirPath: LocalDirectory =>
    (dirPath.isRelative, s"${dirPath} is not relative")
  }


}
