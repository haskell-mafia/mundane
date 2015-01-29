package com.ambiata.mundane.io

import com.ambiata.disorder._
import com.ambiata.mundane.control._
import com.ambiata.mundane.io.Arbitraries._
import com.ambiata.mundane.io.Temporary._
import com.ambiata.mundane.io.LocalPath._
import com.ambiata.mundane.path._
import com.ambiata.mundane.path.Arbitraries._
import com.ambiata.mundane.testing.RIOMatcher._
import java.io.File
import java.net.URI

import org.scalacheck._
import org.specs2._
import org.specs2.matcher.Matcher

import scalaz._, Scalaz._

class LocalDirectorySpec extends Specification with ScalaCheck { def is = s2"""

 Paths are of 2 sorts:

  - file paths
  - directory paths

 The essential difference is only directory paths can be appended with a path name and become a LocalFile.

 They also map to the notion of file and directory on a filesystem (but not on the Java notion of File which can represent both)

 LocalDirectories
 ================

 A LocalDirectory can be created from
   a String

     ${ LocalDirectory.fromString("hello/world").map(_.path.path) === Some("hello/world")  }

   a File

     ${ LocalDirectory.fromFile(new File("/hello/world")).path.path === "/hello/world"  }

   a URI

     ${ LocalDirectory.fromURI(new URI("hello/world")).map(_.path.path) === Some("hello/world")  }

     ${ LocalDirectory.fromURI(new URI("hdfs://100.100.1:9000/hello/world")).map(_.path.path) === Some("/hello/world") }

     ${ LocalDirectory.fromURI(new URI("hdfs:/hello/world")).map(_.path.path) === Some("/hello/world")  }

     ${ LocalDirectory.fromURI(new URI("file:/hello/world")).map(_.path.path) === Some("/hello/world")  }

     ${ LocalDirectory.fromURI(new URI("s3://hello/world")).map(_.path.path) === Some("/world")  }

 An absolute dir path can be built from
   a string starting with a /
   ${ LocalDirectory.fromString("/hello/world").exists(_.path.isAbsolute) }
   the LocalDirectory.Root object
   ${ (LocalDirectory.Root /- "world").isAbsolute }
   appending a LocalDirectory to the Root
   ${ (LocalDirectory.Root / (LocalDirectory.Relative /- "world")).isAbsolute }
   // this combination is accepted but should not be valid...
   ${ (LocalDirectory.Root / (LocalDirectory.Root /- "world")).isAbsolute }

 A relative dir path can be built from
   a string not starting with a
   ${ LocalDirectory.fromString("hello/world").exists(_.path.isRelative) }
   the LocalDirectory.Relative object
   ${ (LocalDirectory.Relative /- "world").isRelative }
   a literal string
   { ("hello" | "world").isRelative } //nh

 Basic operations can be executed on a LocalDirectory
   get the parent
   ${ LocalDirectory.Root.parent must beNone }
   ${ LocalDirectory.unsafe("test").parent.map(_.path) ==== LocalDirectory.Relative.some }
   ${ (LocalDirectory.Root /- "test").parent ==== Some(LocalDirectory.Root) } // TODO how does this type check

   get the path as a string
   ${ LocalDirectory.Root.path must_== "/" }
   ${ LocalDirectory.unsafe("test").path.path must_== "test" }

 IO
 ==

 List
  files
   ${ prop((v: Component, local: LocalTemporary) => for {
        d <- local.directory
        _ <- (d.toLocalPath | v).touch
        r <- d.listFilesRelativeTo.map(_.map(_._2.basename))
      } yield r ==== List(v.some)) }

 List recursively
  files
   ${ prop((v: Component, local: LocalTemporary) => for {
        d <- local.directory
        _ <- (d.toLocalPath | v | v).touch
        r <- d.listRecursivelyRelativeTo.map(_.map(_._2))
      } yield r ==== List(LocalPath(Path(v.name) | v))) }

"""
}
