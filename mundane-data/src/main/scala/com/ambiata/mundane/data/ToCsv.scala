package com.ambiata.mundane.data

trait ToCsv[A] {
  /** @return a comma separated list of values */
  def toCsv(a: A): String
}
