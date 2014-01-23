package com.ambiata.mundane
package data

object Lists {
  def prepareForFile(xs: List[String]) = xs match {
    case Nil => ""
    case xs  => xs.mkString("\n") + "\n"
  }
}
