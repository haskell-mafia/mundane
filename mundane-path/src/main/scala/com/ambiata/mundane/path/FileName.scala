package com.ambiata.mundane.path

import com.ambiata.mundane.reflect.MacrosCompat
import scalaz._, Scalaz._
import java.util.UUID

/**
 * The component of a path name according to the unix definition
 *   http://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap03.html#tag_03_267
 */
class FileName private (val name: String) extends AnyVal

object FileName {
  def create(s: String): Option[FileName] =
    (!s.isEmpty && !s.contains('/')).option(new FileName(s))

  def unsafe(s: String): FileName =
    new FileName(s)

  def fromUUID(uuid: UUID) =
    new FileName(uuid.toString)

  def apply(s: String): FileName =
    macro Macros.foo

  object Macros extends MacrosCompat {
    def foo(c: Context)(s: c.Expr[String]): c.Expr[FileName] = {
      import c.universe._
      s match {
        case Expr(Literal(Constant(v: String))) =>
          create(v) match {
            case Some(s) =>
              c.Expr(q"FileName.unsafe(${s.name})")
            case None =>
              c.abort(c.enclosingPosition, s"$s is not a valid FileName")
          }
        case _ =>
          c.abort(c.enclosingPosition, s"Not a literal ${showRaw(s)}")
      }

    }
  }
}
