package com.ambiata.mundane.reflect

/**
 * Macro to display case classes instances with their field names
 *
 * Usage
 *
 * case class MyCaseClass(field1: Int, field2: Int,...) {
 *   override def toString: String =
 *     macro ToString.toStringWithNames
 *
 * }
 */
object ToString extends MacrosCompat {

  def toStringWithNames(c: Context): c.Expr[String] = {
    import c.universe.{getClass =>_,_}

    // class for which we want to display toString
    val klass = c.macroApplication.symbol.owner
    val className = simpleName(klass.name.decodedName.toString)

    // we keep the getter fields created by the user
    val fields: Iterable[c.Symbol] = klass.asClass.toType.members
      .filter(_.isPublic).
      filter(isGetter(c)).
      filterNot(isSynthetic(c)).
      filter(_.owner != typeOf[Any].typeSymbol).
      filter(_.owner != typeOf[Object].typeSymbol).
      filter(_.owner != typeOf[Product].typeSymbol).
      filter(_.owner != typeOf[Equals].typeSymbol)

    /** some useful literals */
    val equal = q"""" = """"
    val emptyString: String = ""
    val empty = q"""$emptyString"""
    val tab = q""" "  " """
    val newline = q""" "\n" """

    // print one field as <name of the field>+"="+fieldName
    def printField(field: Symbol) = {
      val fieldName = field.name.decodedName.toString
      q"""$fieldName+$equal+${Select(c.prefix.tree, createTermName(c)(fieldName))} """
    }

    // fold over the fields to create an expression like
    // "" +
    // <name of the field1>+"="+fieldName1 +
    // <name of the field1>+"="+fieldName2 + ...
    val parameters = fields.foldLeft(q"$empty") { (res, field) => q"$tab + ${printField(field)} + $newline + $res" }

    // print the class and all the parameters with their values
    c.Expr(q"""$className + "(\n" + $parameters + ")" """)
  }

  def simpleName(name: String): String =
    name.split("\\.").lastOption.getOrElse("<none>").split("\\$").headOption.getOrElse("<none>")

  private def isSynthetic(c: Context) = { import c.universe._
    (s: Symbol) => s.isSynthetic
  }

  private def isGetter(c: Context) = { import c.universe._
    (s: Symbol) =>  s match {
      case m: c.universe.MethodSymbol => m.isGetter
      case other                      => false
    }
  }

}

