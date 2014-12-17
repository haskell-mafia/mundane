package com.ambiata.mundane.io

import com.ambiata.mundane.path._
import java.io.File
import java.net.URI

import org.specs2.Specification
import org.specs2.matcher.Matcher

class LocalFileSpec extends Specification { def is = s2"""

 Paths are of 2 sorts:

  - file paths
  - directory paths

 The essential difference is only directory paths can be appended with a path name and become a LocalFile.

 They also map to the notion of file and directory on a filesystem (but not on the Java notion of File which can represent both)

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
/*
  def beRelative(implicit p1: ImplicitParam1): Matcher[LocalFile] = { filePath: LocalFile =>
    (filePath.isRelative, s"${filePath} is not relative")
  }

  def beAbsolute(implicit p1: ImplicitParam1): Matcher[LocalFile] = { filePath: LocalFile =>
    (filePath.isAbsolute, s"${filePath} is not absolute")
  }
*/
}
