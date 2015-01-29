package com.ambiata.mundane.path

sealed trait WriteMode {
  def fold[X](
    append: => X
  , overwrite: => X
  , fail: => X
  ): X = this match {
    case WriteMode.Append =>
      append
    case WriteMode.Overwrite =>
      overwrite
    case WriteMode.Fail =>
      fail
  }
}

object WriteMode {
  case object Append extends WriteMode
  case object Overwrite extends WriteMode
  case object Fail extends WriteMode
}
