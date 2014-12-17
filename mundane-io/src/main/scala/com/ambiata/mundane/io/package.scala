package com.ambiata.mundane

import path._
import java.io._
import scalaz._, Scalaz._
import scalaz.effect.IO
import control.{ResultT, RIO}

package object io {

  type Logger = String => IO[Unit]
  val noLogging = (s: String) => IO(())
  val consoleLogging = (s: String) => IO(println(s))

  type Env = Map[String, String]

  implicit class LocalFileAsStream(filePath: LocalFile) {
    def toInputStream: RIO[InputStream] = RIO.safe { new FileInputStream(filePath.path.path) }
  }

  implicit class LocalPathSyntax(filePath: Path) {
    def toFile: File = new File(filePath.path)
    def toOutputStream: RIO[OutputStream] = RIO.safe { new FileOutputStream(filePath.path) }
  }

  implicit class LocalFileListSyntax(l: List[LocalFile]) {
    def filterHidden: List[LocalFile] =
      l.filter(f => !List(".", "_").exists(c => f.toPath.basename.exists(_.name.startsWith(c))))
  }
}
