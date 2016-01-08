package com.ambiata.mundane.io

import com.ambiata.mundane.path._
import java.util.UUID

import scalaz._, Scalaz._

object Temporary {
  def tempUniquePath: Component =
    Component.unsafe(s"temporary-${UUID.randomUUID()}")

  def uniqueLocalPath: LocalPath =
    LocalPath(Path(System.getProperty("java.io.tmpdir", "/tmp")) | tempUniquePath)

  def print: Boolean =
    sys.env.exists({ case (k, _) => k === "TEST_PRINT_PATHS" })

  def skipCleanup: Boolean =
    sys.env.exists({ case (k, _) => k === "TEST_SKIP_CLEANUP_RESOURCES" })
}
