package com.ambiata.mundane
package io

import java.util.UUID

object Temporary {
  def tempUniquePath: LocalDirectory =
    LocalDirectory.unsafe(s"temporary-${UUID.randomUUID()}")

  def uniqueLocalDirectory: LocalDirectory =
    LocalDirectory.unsafe(System.getProperty("java.io.tmpdir", "/tmp")) </> tempUniquePath

  def uniqueLocalFile: LocalFile =
    LocalDirectory.unsafe(System.getProperty("java.io.tmpdir", "/tmp")) </> tempUniquePath
}
