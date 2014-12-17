package com.ambiata.mundane.io

import com.ambiata.mundane.path._

case class PathStringSyntax(s: String) {
  def toPath: Path =
    Path(s)

  def </>(s: String): Path =
    toPath </> Path(s)
}
