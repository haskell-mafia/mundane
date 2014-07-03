package com.ambiata.mundane
package io

import com.ambiata.mundane.testing.ResultTIOMatcher._

import org.specs2._
import org.scalacheck._

import scalaz._, Scalaz._

class DirectoriesSpec extends Specification with ScalaCheck { def is = s2"""

Directories
-----------

  should list all files                     $list
  should recursively delete all files       $delete
  should determine if directory exists      $exists
  should determine if directory not exists  $notExists

"""

  def paths(files: List[FilePath]): List[FilePath] =
    files.map(_.absolute).sortBy(_.path)

  def list =
    prop((tree: FileTree) => Temporary.using { tmp =>
      val base = tmp </> "base"
      val action = tree.create(base) >> Directories.list(base)
      action.map(paths) map(_ == paths(tree.files(base)))
    } must beOkValue(true))

  def delete =
    prop((tree: FileTree) => Temporary.using { tmp =>
      val base = tmp </> "base"
      tree.create(base) >> Directories.delete(base) >> Directories.exists(base)
    } must beOkValue(false))

  def exists =
    Temporary.using { tmp =>
      val base = tmp </> "base"
      Directories.mkdirs(base) >> Directories.exists(base)
    } must beOkValue(true)

  def notExists =
    Temporary.using { tmp =>
      val base = tmp </> "base"
      Directories.exists(base)
    } must beOkValue(false)
}
