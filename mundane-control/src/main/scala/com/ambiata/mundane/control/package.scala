package com.ambiata.mundane

import scalaz._, Scalaz._
import scalaz.effect._

package object control {
  type RIO[A] = ResultT[IO, A]
  type AIO[W, R, A] = ActionT[IO, W, R, A]
  type Action[W, R, A] = ActionT[Id, W, R, A]
}
