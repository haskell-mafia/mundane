package com.ambiata.mundane.data

import scalaz.Zipper

/**
 * Utility methods for Zippers
 */
object Zipperx {
  implicit class ZipperOps[T](zipper: Zipper[T]) {
    def rest = zipper.lefts ++ zipper.rights
  }
}
