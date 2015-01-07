package com.ambiata.mundane.io

import scalaz._, Scalaz._

object Temporary {
  def tempUniquePath: DirPath =
    DirPath.unsafe(s"temporary-${java.util.UUID.randomUUID()}")

  def uniqueDirPath: DirPath =
    DirPath.unsafe(System.getProperty("java.io.tmpdir", "/tmp")) </> tempUniquePath

  def uniqueFilePath: FilePath =
    DirPath.unsafe(System.getProperty("java.io.tmpdir", "/tmp")) </> tempUniquePath.toFilePath

  def print: Boolean =
    sys.env.exists({ case (k, _) => k === "TEST_PRINT_PATHS" })

  def skipCleanup: Boolean =
    sys.env.exists({ case (k, _) => k === "TEST_SKIP_CLEANUP_RESOURCES" })
}
