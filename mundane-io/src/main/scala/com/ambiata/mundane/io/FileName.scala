package com.ambiata.mundane.io

import scalaz._, Scalaz._
import java.util.UUID

/**
 * The component of a path name according to the unix definition
 *   http://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap03.html#tag_03_267
 */
case class FileName private (name: String)

object FileName {
  def create(s: String): Option[FileName] =
    (!s.isEmpty && !s.contains('/')).option(FileName(s))

  def unsafe(s: String): FileName =
    FileName(s)

  def fromUUID(uuid: UUID) =
    FileName(uuid.toString)

  def Parent =
    FileName.unsafe("..")

  def Current =
    FileName.unsafe(".")
}
