package com.ambiata.mundane.store
package io

import scalaz._, Scalaz._, \&/._, effect.IO
import org.specs2._, specification.AfterExample
import com.ambiata.mundane.control._
import com.ambiata.mundane.io._
import com.ambiata.mundane.testing.ResultTIOMatcher._
import java.io.{File, FileOutputStream, ByteArrayInputStream}
import java.util.UUID

class StoreSpec extends Specification with ScalaCheck { def is = isolated ^ s2"""
  Store Usage
  ==========

  """
}
