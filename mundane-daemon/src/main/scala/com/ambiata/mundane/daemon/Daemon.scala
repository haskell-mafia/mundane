package com.ambiata.mundane.daemon

import scalaz._, Scalaz._
import scalaz.effect._

trait Daemon[A] {
  def env: String \/ A

  def run(conf: A): IO[Unit]

  def main(args: Array[String]): Unit =
    env match {
      case -\/(field) =>
        println(s"Missing or invalid field [${field}].")
        sys.exit(1)
      case \/-(conf) =>
        def forever = run(conf).ensuring(forever)
        forever.unsafePerformIO
    }
}
