package com.ambiata.mundane
package io

import java.io._
import org.specs2._
import org.scalacheck._, Arbitrary._

// FIX finish this, need to add fold to file tree so it is easy to work with.
class DirectoriesSpec extends Specification with ScalaCheck { def is = s2"""

Directories
-----------

  should list all files                  $list
  should recursively delete all files    $delete

"""
  lazy val tmp = new File(System.getProperty("java.io.tmpdir", "/tmp"))

  def list =
    prop((tree: FileTree, n: Int) => pending)

  def delete =
    prop((tree: FileTree) => pending)
}
