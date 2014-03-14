package com.ambiata.mundane

import scalaz._, Scalaz._
import scalaz.effect.IO
import com.ambiata.mundane.control.{ActionTSupport, ActionT}

package object io {
  type Logger = String => IO[Unit]
  lazy val noLogging = (s: String) => IO(())
  lazy val consoleLogging = (s: String) => IO(println(s))

  type IOAction[+A] = ActionT[IO, Unit, Logger, A]
  object IOActions extends ActionTSupport[IO, Unit, Logger]

  type Env = Map[String, String]

  /** log a value, using the logger coming from the Reader environment */
  def log[R](r: R): IOAction[Unit] =
    IOActions.ask.flatMap(logger => logger(r.toString).liftIO[IOAction])
}
