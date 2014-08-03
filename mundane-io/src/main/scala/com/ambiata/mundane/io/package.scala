package com.ambiata.mundane

import java.io._
import reflect.MacrosCompat
import scalaz._, Scalaz._
import scalaz.effect.IO
import control.{ResultT, ActionTSupport, ActionT}

package object io extends MacrosCompat {

  type Logger = String => IO[Unit]
  lazy val noLogging = (s: String) => IO(())
  lazy val consoleLogging = (s: String) => IO(println(s))

  type IOAction[A] = ActionT[IO, Unit, Logger, A]
  object IOActions extends ActionTSupport[IO, Unit, Logger]

  type Env = Map[String, String]

  /** log a value, using the logger coming from the Reader environment */
  def log[R](r: R): IOAction[Unit] =
    IOActions.ask.flatMap(logger => logger(r.toString).liftIO[IOAction])

  implicit def stringToDirPathSyntax(s: String): DirPathSyntax =
    macro createDirPathSyntax

  def createDirPathSyntax(c: Context)(s: c.Expr[String]): c.Expr[DirPathSyntax] = {
    import c.universe._
    s match {
      case Expr(Literal(Constant(v: String))) => c.Expr(q"new DirPathSyntax(${createFileNameFromString(c)(v)})")
      case _ => c.abort(c.enclosingPosition, s"Not a literal ${showRaw(s)}")
    }

  }

  implicit class NameToDirPathSyntax(name: FileName) {
    def </>(other: FileName): DirPath  = DirPath(name) </> other
    def <|>(other: FileName): FilePath = DirPath(name) <|> other
  }

  implicit class FilePathAsStream(filePath: FilePath) {
    def toOutputStream: ResultT[IO, OutputStream] = ResultT.safe { new FileOutputStream(filePath.path) }
    def toInputStream: ResultT[IO, InputStream] = ResultT.safe { new FileInputStream(filePath.path) }
  }

  implicit class FilePathListSyntax(l: List[FilePath]) {
    def filterHidden: List[FilePath] =
      l.filter(f => !Seq(".", "_").exists(c => f.basename.name.startsWith(c)))
  }

  implicit class DirPathListSyntax(l: List[DirPath]) {
    def filterHidden: List[DirPath] =
      l.filter(f => !f.basename.name.startsWith("."))
  }

  implicit def ToFileName(s: String): FileName =
    macro create

  def fromString(s: String): Option[FileName] =
    if (s.contains("/")) None
    else Some(FileName.unsafe(s))

  def create(c: Context)(s: c.Expr[String]): c.Expr[FileName] = {
    import c.universe._
    s match {
      case Expr(Literal(Constant(v: String))) => createFileNameFromString(c)(v)
      case _ => c.abort(c.enclosingPosition, s"Not a literal ${showRaw(s)}")
    }
  }

  private def createFileNameFromString(c: Context)(s: String): c.Expr[FileName] = {
    import c.universe._
    fromString(s) match {
      case None     => c.abort(c.enclosingPosition, s"$s is not a valid fileName. It must not contain a /")
      case Some(fn) => c.Expr(q"FileName.unsafe(${fn.name})")
    }
  }
}


