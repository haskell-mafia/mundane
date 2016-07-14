package com.ambiata.mundane.io

import com.ambiata.disorder._
import com.ambiata.mundane.control._
import com.ambiata.mundane.io.Arbitraries._
import com.ambiata.mundane.path._
import com.ambiata.mundane.path.Arbitraries._
import com.ambiata.mundane.testing.RIOMatcher._
import java.io.File
import java.net.URI

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

     ${ LocalDirectory.unsafe("hello/world").path.path === "hello/world"  }

   a Directory

     ${ prop((l: LocalTemporary) => for {
          p <- l.path
          f <- p.touch
          d <- p.dirname.determineDirectory
          n <- LocalDirectory.fromFile(d.toFile)
        } yield d ==== n)
      }

   a URI

     ${ prop((l: LocalTemporary) => for {
          p <- l.path
          f <- p.touch
          d <- p.dirname.determineDirectory
          u = new File(d.path.path).toURI()
          n <- LocalDirectory.fromURI(u)
        } yield d.some ==== n)
      }

 A LocalDirectory can be ordered

   ${ List(LocalDirectory.unsafe("z"), LocalDirectory.unsafe("a")).sorted ==== List(LocalDirectory.unsafe("a"), LocalDirectory.unsafe("z")) }

 IO
 ==

  LocalFile should be able to perform these basic operations

    Check if a directory exists

      ${ prop((l: LocalTemporary) => l.directory.flatMap(_.exists) must beOkValue(true)) }

      ${ LocalDirectory.unsafe("test").exists must beOkValue(false) }

      ${ prop((l: LocalTemporary) => { var i = 0; l.directory.flatMap(_.whenExists(RIO.io({ i = 1; i }).void)).as(i) must beOkValue(1) }) }

      ${ prop((l: LocalTemporary) => { var i = 0; l.directory.flatMap(_.doesExist("", RIO.io({ i = 1; i }).void)).as(i) must beOkValue(1) }) }

      ${ prop((l: LocalTemporary) => l.directory.flatMap(_.doesNotExist("", RIO.unit)) must beFail) }

      ${ var i = 0; LocalDirectory.unsafe("test").doesNotExist("", RIO.io({ i = 1; i })) must beOkValue(1) }

    Delete a directory

      ${ prop((l: LocalTemporary) => l.directory.flatMap(d => d.delete >> d.exists) must beOkValue(false)) }


  LocalDirectory should be able to move a directory handling failure cases gracefully

    ${ prop((l: LocalTemporary) => for {
         p <- l.path
         d <- l.directory
         n <- d.move(p)
         b <- d.exists
         a <- n.exists
       } yield b -> a ==== false -> true)
     }

    ${ prop((v: DistinctPair[Component], local: LocalTemporary) => for {
         p <- local.path
         d <- local.directory
         _ <- (d.toLocalPath | v.first | v.second).touch
         _ <- (d.toLocalPath | v.first | v.first | v.second).touch
         _ <- (d.toLocalPath | v.second).touch
         n <- d.move(p)
         r <- n.listPathsRecursively.map(_.map(_.path.rebaseTo(d.path)))
         e <- (n.toLocalPath | v.first | v.first | v.second).exists
       } yield r.size -> e ==== 5 -> true)
     }

    ${ prop((l: LocalTemporary) => (for {
         p <- l.directory
         d <- l.directory
         n <- d.move(p.toLocalPath)
       } yield ()) must beFail)
     }

    ${ prop((l: LocalTemporary) => (for {
         p <- l.path
         d =  LocalDirectory.unsafe("test")
         n <- d.move(p)
       } yield ()) must beFail)
     }

    ${ prop((l: LocalTemporary) => (for {
         p <- l.path
         d =  LocalDirectory.unsafe("/")
         n <- d.move(p)
       } yield ()) must beFailWithMessage("Source is a top level directory, can't move"))
     }

    Move a directory with respect to different TargetMode's

      ${ prop((l: LocalTemporary) => for {
           p <- l.directory
           d <- l.directory
           n <- d.moveWithMode(p.toLocalPath, TargetMode.Overwrite)
           b <- d.exists
           a <- n.exists
         } yield b -> a ==== false -> true)
       }

      ${ prop((l: LocalTemporary) => (for {
           p <- l.directory
           d <- l.directory
           n <- d.moveWithMode(p.toLocalPath, TargetMode.Fail)
         } yield ()) must beFail)
       }

    Move a directory to another directory, placing the original directory inside the target directory

      ${ prop((l: LocalTemporary) => for {
           p <- l.directory
           d <- l.directory
           n <- d.moveTo(p)
           z <- d.exists
           r <- n.exists
         } yield z -> r ==== false -> true)
       }

      ${ prop((l: LocalTemporary) => for {
           p <- l.directory
           d <- l.directory
           n <- d.moveTo(p)
         } yield n.toLocalPath ==== (p.toLocalPath | d.path.basename.get))
       }


  LocalDirectory should be able to list files

    List files relative to the directory

      ${ prop((v: DistinctPair[Component], local: LocalTemporary) => for {
           d <- local.directory
           _ <- (d.toLocalPath | v.first).touch
           _ <- (d.toLocalPath | v.second).mkdirs
           r <- d.listFilesRelativeTo.map(_.map(_._2.basename))
         } yield r ==== List(v.first.some))
       }

    List files recursively relative to the directory

      ${ prop((v: Component, local: LocalTemporary) => for {
           d <- local.directory
           _ <- (d.toLocalPath | v | v).touch
           r <- d.listFilesRecursivelyRelativeTo.map(_.map(_._2))
         } yield r ==== List(LocalPath(Path(v.name) | v)))
       }

  LocalDirectory should be able to list directories

    List directories relative to the directory

      ${ prop((v: DistinctPair[Component], local: LocalTemporary) => for {
           d <- local.directory
           _ <- (d.toLocalPath | v.first | v.second).touch
           _ <- (d.toLocalPath | v.second).touch
           r <- d.listDirectories.map(_.map(_.toLocalPath.basename))
         } yield r ==== List(v.first.some))
       }

    List directories recursively relative to the directory

      ${ prop((v: DistinctPair[Component], local: LocalTemporary) => for {
           d <- local.directory
           _ <- (d.toLocalPath | v.first | v.second).touch
           _ <- (d.toLocalPath | v.first | v.first | v.second).touch
           _ <- (d.toLocalPath | v.second).touch
           r <- d.listDirectoriesRecursively.map(_.map(_.path.rebaseTo(d.path)))
         } yield r ==== List(Path(v.first.name).some, (Path(v.first.name) | v.first).some))
       }

  LocalDirectory should be able to list paths which represent both files and directories

    List paths in the given directory

      ${ prop((v: DistinctPair[Component], local: LocalTemporary) => for {
           d <- local.directory
           _ <- (d.toLocalPath | v.first | v.second).touch
           _ <- (d.toLocalPath | v.second).touch
           r <- d.listPaths.map(_.map(_.path.rebaseTo(d.path)))
         } yield r.sorted ==== List(Path(v.second.name), Path(v.first.name)).map(_.some).sorted)
       }

    List paths recrusively

      ${ prop((v: DistinctPair[Component], local: LocalTemporary) => for {
           d <- local.directory
           _ <- (d.toLocalPath | v.first | v.second).touch
           _ <- (d.toLocalPath | v.first | v.first | v.second).touch
           _ <- (d.toLocalPath | v.second).touch
           r <- d.listPathsRecursively.map(_.map(_.path.rebaseTo(d.path)))
         } yield r.sorted ==== List(Path(v.first.name), Path(v.second.name), Path(v.first.name) | v.second,
            Path(v.first.name) | v.first, Path(v.first.name) | v.first | v.second).map(_.some).sorted)
       }

"""
}
