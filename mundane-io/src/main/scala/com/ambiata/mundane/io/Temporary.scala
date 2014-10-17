package com.ambiata.mundane
package io

import java.util.UUID

object Temporary {
  def tempUniquePath: DirPath =
    DirPath.unsafe(s"temporary-${UUID.randomUUID()}")

  def uniqueDirPath: DirPath =
    DirPath.unsafe(System.getProperty("java.io.tmpdir", "/tmp")) </> tempUniquePath

  def uniqueFilePath: FilePath =
    DirPath.unsafe(System.getProperty("java.io.tmpdir", "/tmp")) </> tempUniquePath
}
