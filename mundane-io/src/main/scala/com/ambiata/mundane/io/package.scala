package com.ambiata.mundane

import path._
import java.io._
import scalaz._, Scalaz._
import scalaz.effect.IO
import control.{ResultT, RIO}

package object io {
  type LocalFile = Path
  type LocalDirectory = Path

  type Logger = String => IO[Unit]
  val noLogging = (s: String) => IO(())
  val consoleLogging = (s: String) => IO(println(s))

  type Env = Map[String, String]

  implicit class FilePathAsStream(filePath: LocalFile) {
    def toOutputStream: RIO[OutputStream] = RIO.safe { new FileOutputStream(filePath.path) }
    def toInputStream: RIO[InputStream] = RIO.safe { new FileInputStream(filePath.path) }
  }

  implicit class LocalFileListSyntax(l: List[LocalFile]) {
    def filterHidden: List[LocalFile] =
      l.filter(f => !List(".", "_").exists(c => f.basename.exists(_.name.startsWith(c))))
  }

  implicit class LocalFileStringSyntax(l: String) {
//    def </(n: FileName): LocalFile =
//      LocalDirectory.Relative </ FileName.unsafe(l) </ n
    def </(n: String): LocalFile =
      LocalDirectory.Relative </ FileName.unsafe(l) </ FileName.unsafe(n)
  }
}
