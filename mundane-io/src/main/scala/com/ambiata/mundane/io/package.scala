package com.ambiata.mundane

import java.io._
import reflect.MacrosCompat
import scalaz._, Scalaz._
import scalaz.effect.IO
import control.{ResultT, RIO}

package object io extends MacrosCompat {
  type P = Path
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

  implicit def ToFileName(s: String): FileName =
    macro create

  def create(c: Context)(s: c.Expr[String]): c.Expr[FileName] = {
    import c.universe._
    s match {
      case Expr(Literal(Constant(v: String))) => createFileNameFromString(c)(v)
      case _ => c.abort(c.enclosingPosition, s"Not a literal ${showRaw(s)}")
    }
  }

  private def createFileNameFromString(c: Context)(s: String): c.Expr[FileName] = {
    def fromString(s: String): Option[FileName] =
      if (s.contains("/")) None
      else Some(FileName.unsafe(s))


    import c.universe._
    fromString(s) match {
      case None     => c.abort(c.enclosingPosition, s"$s is not a valid fileName. It must not contain a /")
      case Some(fn) => c.Expr(q"FileName.unsafe(${fn.name})")
    }
  }
}
