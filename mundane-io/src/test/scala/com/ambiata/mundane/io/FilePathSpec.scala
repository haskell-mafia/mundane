package com.ambiata.mundane.io

import java.io.File
import java.net.URI

import org.specs2.Specification

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
   ${ DirPath.unsafe("hello/world").path === "hello/world"  }
   a File
   ${ DirPath.unsafe(new File("hello/world")).path === "hello/world"  }
   a URI
   ${ DirPath.unsafe(new URI("hello/world")).path === "hello/world"  }

 The DirPath loses its scheme if created from a string/file/uri
   ${ DirPath.unsafe(new URI("s3://hello/world")).path === "hello/world"  }

 Basic operations can be executed on a DirPath
   get the parent
   ${ DirPath.Root.parent must beNone }
   ${ DirPath("test").parent must beSome(DirPath.Root) }
   ${ ("test" </> "hello" </> "world").parent must beSome("test" </> "hello") }

   get the basename
   ${ ("test" </> "hello" </> "world").basename === FileName.unsafe("world") }

   get the rootname
   ${ ("test" </> "hello" </> "world").rootname must_== DirPath.unsafe("test") }

   get the path as a string
   ${ ("test" </> "hello" </> "world").path must_== "test/hello/world" }

   get a portion of the path
   ${ ("test" </> "hello" </> "world" </> "eric").relativeTo("test" </> "hello")  === "world" </> "eric" }
   ${ ("test" </> "hello" </> "world" </> "eric").relativeTo("other" </> "hello") === "test" </> "hello" </> "world" </> "eric" }
   ${ ("test" </> "hello" </> "world").fromRoot === "hello" </> "world" }
"""
  val x = tag("x")
}
