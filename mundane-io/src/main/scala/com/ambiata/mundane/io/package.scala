package com.ambiata.mundane

import path._
import java.io._
import scalaz.effect.IO
import control.{ResultT, RIO}

package object io {

  implicit class LocalPathSyntax(filePath: Path) {
    def toFile: File = new File(filePath.path)
    def toOutputStream: RIO[OutputStream] = RIO.safe { new FileOutputStream(filePath.path, true) }
    def toOverwriteOutputStream: RIO[OutputStream] = RIO.safe { new FileOutputStream(filePath.path, false) }
  }

}
