package com.ambiata.mundane

import path._
import java.io._
import scalaz._, Scalaz._
import scalaz.effect.IO
import control.{ResultT, RIO}

package object io {
  type FilePath = P
  type DirPath = P

  type Logger = String => IO[Unit]
  val noLogging = (s: String) => IO(())
  val consoleLogging = (s: String) => IO(println(s))

  type Env = Map[String, String]

  implicit class FilePathAsStream(filePath: FilePath) {
    def toOutputStream: RIO[OutputStream] = RIO.safe { new FileOutputStream(filePath.path) }
    def toInputStream: RIO[InputStream] = RIO.safe { new FileInputStream(filePath.path) }
  }

  implicit class FilePathListSyntax(l: List[FilePath]) {
    def filterHidden: List[FilePath] =
      l.filter(f => !List(".", "_").exists(c => f.basename.exists(_.name.startsWith(c))))
  }

  implicit class FilePathStringSyntax(l: String) {
    def </(n: FileName): FilePath =
      DirPath.Relative </ FileName.unsafe(l) </ n
    def </(n: String): FilePath =
      DirPath.Relative </ FileName.unsafe(l) </ FileName.unsafe(n)
  }
}
