package com.ambiata.mundane

import java.io._
import reflect.MacrosCompat
import scalaz._, Scalaz._
import scalaz.effect.IO
import control.{ResultT, ActionTSupport, ActionT}

package object path extends MacrosCompat {
  type P = Path

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
