package com.ambiata.mundane.io

/**
 * The component of a path name according to the unix definition
 *   http://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap03.html#tag_03_267
 */
case class FileName private(name: String)

object FileName {
  def unsafe(s: String) = new FileName(s)
}


