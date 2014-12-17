package com.ambiata.mundane.io

import com.ambiata.mundane.path._
import java.util.UUID

object Temporary {
  def tempUniquePath: Component =
    Component.unsafe(s"temporary-${UUID.randomUUID()}")

  def uniqueLocalPath: Path =
    Path(System.getProperty("java.io.tmpdir", "/tmp")) </ tempUniquePath
}
