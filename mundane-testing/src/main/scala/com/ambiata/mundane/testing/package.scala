package com.ambiata.mundane

import scalaz.effect.IO
import org.specs2.execute.AsResult

package object testing {

  /**
   * This implicit allows any IO[result] to be used inside an example:
   *
   * "this should work" in {
   *   IO(success)
   * }
   *
   */
  implicit def ioResultAsResult[T : AsResult]: AsResult[IO[T]] = new AsResult[IO[T]] {
    def asResult(io: =>IO[T]) = AsResult(io.unsafePerformIO())
  }
}
