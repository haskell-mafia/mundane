package com.ambiata.mundane.path

import com.ambiata.mundane.reflect.MacrosCompat
import scalaz._, Scalaz._
import java.util.UUID

/**
 * The component of a path name according to the unix definition
 *   http://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap03.html#tag_03_267
 */
class Component private (val name: String) extends AnyVal {
  override def toString: String =
    s"Component($name)"
}

object Component {
  def create(s: String): Option[Component] =
    (!s.isEmpty && !s.contains('/')).option(new Component(s))

  def unsafe(s: String): Component =
    new Component(s)

  def fromUUID(uuid: UUID) =
    new Component(uuid.toString)

  def Parent =
    Component.unsafe("..")

  def Current =
    Component.unsafe(".")

  def apply(s: String): Component =
    macro Macros.foo

  object Macros extends MacrosCompat {
    def foo(c: Context)(s: c.Expr[String]): c.Expr[Component] = {
      import c.universe._
      s match {
        case Expr(Literal(Constant(v: String))) =>
          create(v) match {
            case Some(s) =>
              c.Expr(q"Component.unsafe(${s.name})")
            case None =>
              c.abort(c.enclosingPosition, s"$s is not a valid Component")
          }
        case _ =>
          c.abort(c.enclosingPosition, s"Not a literal ${showRaw(s)}")
      }

    }
  }

  implicit def ComponentOrder: Order[Component] =
    Order.order((x, y) => x.name.?|?(y.name))

  implicit def ComponentOrdering =
    ComponentOrder.toScalaOrdering
}
