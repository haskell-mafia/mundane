package com.ambiata.mundane.io

case class PathStringSyntax(s: String) {
  def toPath: Path =
    Path(s)

  def </>(s: String): Path =
    toPath </> Path(s)
}
