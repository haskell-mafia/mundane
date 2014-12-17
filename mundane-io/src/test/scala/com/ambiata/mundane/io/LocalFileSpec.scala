package com.ambiata.mundane.io

import com.ambiata.mundane.path._
import java.io.File
import java.net.URI

import org.specs2._
import org.scalacheck._
import Arbitraries._

class LocalFileSpec extends Specification with ScalaCheck { def is = s2"""

 Paths are of 2 sorts:

  - file paths
  - directory paths

 The essential difference is only directory paths can be appended with a path name and become a LocalFile.

 They also map to the notion of file and directory on a filesystem (but not on the Java notion of File which can represent both)

 Path
 ====

  'Path.fromFile' is symmetric with Path#toFile:

    ${ prop((p: Path) => LocalFile.fromFile(p.toFile).path ==== p) }

  'Path.fromFile' is consistent with Path.apply:

    ${ prop((p: Path) => LocalFile.fromFile(new java.io.File(p.path)).path ==== p) }


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
   ${ (LocalDirectory.Relative </- "test" </- "hello" </- "world").path must_== "test/hello/world" }
   ${ (LocalDirectory.Relative </- "test" </- "hello" </> LocalDirectory.Relative).path must_== "test/hello" }

   filter hidden files from a list
   ${ List(LocalFile.unsafe("hello/.world"), LocalFile.unsafe("hello/world"), LocalFile.unsafe("hello/_SUCCESS")).filterHidden === List(LocalFile.unsafe("hello/world")) }

"""

}
