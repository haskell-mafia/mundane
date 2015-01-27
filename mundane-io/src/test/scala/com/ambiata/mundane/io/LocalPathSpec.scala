package com.ambiata.mundane.io

import com.ambiata.disorder._
import com.ambiata.mundane.control.{Result => RR, _}
import com.ambiata.mundane.io.Arbitraries._
import com.ambiata.mundane.io.Temporary._
import com.ambiata.mundane.io.LocalPath._
import com.ambiata.mundane.path._
import com.ambiata.mundane.path.Arbitraries._
import com.ambiata.mundane.testing.RIOMatcher._
import MemoryConversions._
import java.io.File
import java.net.URI

import org.specs2._
import org.scalacheck._
import org.specs2.execute.{Error => _, _}
import org.specs2.matcher.Matcher
import org.specs2.matcher.MatchResult
import org.specs2.matcher.DisjunctionMatchers

import scalaz.{Failure => _, _}, Scalaz._

class LocalPathSpec extends Specification with ScalaCheck with DisjunctionMatchers { def is = s2"""

 Paths are of 2 sorts:

  - file paths
  - directory paths

 The essential difference is only directory paths can be appended with a path name and become a LocalFile.

 They also map to the notion of file and directory on a filesystem (but not on the Java notion of File which can represent both)

 LocalIO
 =======

 Can determine things
   a File
   ${ LocalTemporary.random.path.flatMap(path => path.touch >> path.determine) must beOkLike(_ must beFile) }
   a Directory
   ${ LocalTemporary.random.path.flatMap(path => path.mkdirs >> path.determine) must beOkLike(_ must beDirectory) }
   nothing
   ${ LocalTemporary.random.path.flatMap(path => path.determine) must beOkLike(_ must beNone) }
   Empty path
   ${ LocalPath(Path("")).determine must beOkLike(_ must beNone) }

 Can determine file
   a file
   ${ LocalTemporary.random.path.flatMap(path => path.touch >> path.determineFile) must beOk }
   a directory
   ${ LocalTemporary.random.path.flatMap(path => path.mkdirs >> path.determineFile) must beFailWithMessage("Not a valid file") }
   failure
   ${ LocalTemporary.random.path.flatMap(path => path.determineFile) must beFailWithMessage("Not a valid File or Directory") }

 Can determine directory
   a file
   ${ LocalTemporary.random.path.flatMap(path => path.touch >> path.determineDirectory) must beFailWithMessage("Not a valid directory") }
   a directory
   ${ LocalTemporary.random.path.flatMap(path => path.mkdirs >> path.determineDirectory) must beOk }
   failure
   ${ LocalTemporary.random.path.flatMap(path => path.determineDirectory) must beFailWithMessage("Not a valid File or Directory") }

 Exists
   a file
   ${ LocalTemporary.random.path.flatMap(path => path.touch >> path.exists) must beOkValue(true) }
   a directory
   ${ LocalTemporary.random.path.flatMap(path => path.mkdirs >> path.exists) must beOkValue(true) }

 List at a single level
   a single file
   ${ LocalTemporary.random.path.flatMap(path => path.touch >>= (s => path.listFiles.map(s -> _))) must
      beOkLike({ case (l: LocalFile, z: List[LocalFile]) => z ==== List(l) }) }

   'listFiles' is consistent with 'determineFile'
   ${ LocalTemporary.random.path.flatMap(path => path.touch >> path.listFiles >>= ((l: List[LocalFile]) =>
      l.traverse(f => f.toLocalPath.determineFile))) must beOk }

   multiple files
   ${ prop((a: Component, b: Component, c: Component) => (a.name != b.name && a.name != c.name) ==> { LocalTemporary.random.path.flatMap(path =>
      (path | a).touch >> (path | b).touch >> (path | c).mkdirs >> bases(path.listFiles.map(_.sorted))) must
        beOkValue(List(a, b).sorted) }) }

   a directory
   ${ prop((v: DistinctPair[Component]) => LocalTemporary.random.path.flatMap(path => (path | v.first).mkdirs >> (path | v.second).touch >>
      path.listDirectories.map(_.map(_.path.basename))) must beOkValue(List(Some(v.first))) )}

   multiple paths
   { prop((v: Component) => LocalTemporary.random.path.flatMap(path => (path | v | v).touch >>
      path.listPaths.map(_map) must beOkValue( ) }
        beOkLike({ case (expected: List[Path], z: List[Path]) => z ==== expected })) }

 List recursively
  files
   ${ prop((p: DistinctPair[Component]) => { val v = p.first; LocalTemporary.random.path.flatMap(path =>
      List(path | v | v | v | v, path | v | p.second, path | v | v | p.second).traverse(_.touch) >>
        path.listFilesRecursively.map(_.map(_.path.rebaseTo(path.path))).map(_.sorted)) must
          beOkValue(List((Relative | v | v | v | v).some, (Relative | v | p.second).some, (Relative | v | v | p.second).some).sorted) }) }

  directories
   ${ prop((d: DistinctPair[Component], local: LocalTemporary) => for {
        p <- local.path
        x = d.first
        a = p | x | x | x
        b = p | x | x
        c = p | x
        _ <- List(a | x, b | d.second, c | d.second).traverse(_.touch)
        r <- p.listDirectoriesRecursively.map(_.map(_.toLocalPath))
      } yield r.sorted ==== List(a, b, c).sorted) }

  paths
   { prop((v: Component) => LocalTemporary.random.path.flatMap(path => (path | v | v).touch >>
      path.listPathsRecursively must beOkValue(List(path)))) }


        beOkLike({ case (expected: List[Path], z: List[Path]) => z ==== expected })) }

 Size
   a file
   { propNoShrink((v: String) => LocalTemporary.random.path.flatMap(path => write(path, v) >> ssize(path)) must
      beOkValue(v.getBytes.length.bytes)) }

   a directory
   { propNoShrink((f: Component, ff: Component, v: String, vv: String) => LocalTemporary.random.path.flatMap(path =>
      write(path | f, v) >> write(path | ff, vv) >> ssize(path)) must
        beOkValue((v.getBytes.length + vv.getBytes.length).bytes)) }

 Delete
   a file
   { LocalTemporary.random.path.flatMap(path => path.touch >> deleteIt(path) >> path.exists) must
      beOkValue(false) }

   a directory
   { LocalTemporary.random.path.flatMap(path => path.mkdirs >> deleteIt(path) >> path.exists) must
      beOkValue(false) }

 Move
   a file
   { prop((p: DistinctPair[Component]) => { LocalTemporary.random.path.flatMap(path => touch(path | p.first) >>
      move(path | p.first, path | p.second) >> exists(path | p.first) >>= (b =>
        exists(path | p.second).map(b -> _))) must beOkValue(false -> true) })}

   a file to directory
   { prop((p: DistinctPair[Component]) => { LocalTemporary.random.path.flatMap(path => touch(path | p.first) >>
      mkdirs(path | p.second) >> move(path | p.first, path | p.second) >>
        exists(path | p.first) >>= (b =>  exists(path | p.second | p.first).map(b -> _))) must
          beOkValue(false -> true) })}

   a file to file
   { prop((p: DistinctPair[Component]) => { LocalTemporary.random.path.flatMap(path => touch(path | p.first) >>
      touch(path | p.second) >> move(path | p.first, path | p.second)) must beFail })}

   a directory
   { prop((p: DistinctPair[Component], v: Component) => { LocalTemporary.random.path.flatMap(path => mkdirs(path | p.first) >>
      touch(path | p.first | v) >> move(path | p.first, path | p.second) >>
        exists(path | p.first) >>= (e => exists(path | p.second | v).map(e -> _))) must beOkValue(false -> true) })}

   a directory to directory
   { prop((p: DistinctPair[Component], v: Component) => { LocalTemporary.random.path.flatMap(path => mkdirs(path | p.first) >>
      touch(path | p.first | v) >> mkdirs(path | p.second) >> move(path | p.first, path | p.second) >>
        exists(path | p.first) >>= (e => exists(path | p.second | p.first | v).map(e -> _))) must beOkValue(false -> true) })}

   a directory to file
   { prop((p: DistinctPair[Component]) => { LocalTemporary.random.path.flatMap(path => mkdirs(path | p.first) >>
      touch(path | p.second) >> move(path | p.first, path | p.second)) must beFail })}

 Copy
   a file
   { prop((p: DistinctPair[Component]) => { LocalTemporary.random.path.flatMap(path => touch(path | p.first) >>
      copy(path | p.first, path | p.second) >> exists(path | p.first) >>= (b =>
        exists(path | p.second).map(b -> _))) must beOkValue(true -> true) })}

   a file to directory
   { prop((p: DistinctPair[Component]) => { LocalTemporary.random.path.flatMap(path => touch(path | p.first) >>
      mkdirs(path | p.second) >> copy(path | p.first, path | p.second) >>
        exists(path | p.first) >>= (b =>  exists(path | p.second | p.first).map(b -> _))) must
          beOkValue(true -> true) })}

   a file to file
   { prop((p: DistinctPair[Component]) => { LocalTemporary.random.path.flatMap(path => touch(path | p.first) >>
      touch(path | p.second) >> copy(path | p.first, path | p.second)) must beFail })}

   a directory
   { prop((p: DistinctPair[Component]) => { LocalTemporary.random.path.flatMap(path => touch(path | p.first | p.first) >>
      copy(path | p.first, path | p.second) >> exists(path | p.first) >>= (b =>
        exists(path | p.second | p.first).map(b -> _))) must beOkValue(true -> true) })}

   a directory to directory
   { prop((p: DistinctPair[Component]) => { LocalTemporary.random.path.flatMap(path => touch(path | p.first | p.first) >>
      mkdirs(path | p.second) >> copy(path | p.first, path | p.second) >> exists(path | p.first) >>= (b =>
        exists(path | p.second | p.first | p.first).map(b -> _))) must beOkValue(true -> true) })}

   a directory to file
   { prop((p: DistinctPair[Component]) => { LocalTemporary.random.path.flatMap(path => mkdirs(path | p.first) >>
      touch(path | p.second) >> copy(path | p.first, path | p.second)) must beFail })}

"""

  val beFile = beSome(be_-\/[LocalFile])
  val beDirectory = beSome(be_\/-[LocalDirectory])

  def bases(l: RIO[List[LocalFile]]): RIO[List[Component]] =
    l.map(_.map(_.path.basename).flatten)

}
