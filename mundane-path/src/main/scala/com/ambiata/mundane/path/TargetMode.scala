package com.ambiata.mundane.path

sealed trait TargetMode {
  def fold[X](
    overwrite: => X
  , fail: => X
  ): X = this match {
    case TargetMode.Overwrite =>
      overwrite
    case TargetMode.Fail =>
      fail
  }
}

object TargetMode {
  case object Overwrite extends TargetMode
  case object Fail extends TargetMode
}
