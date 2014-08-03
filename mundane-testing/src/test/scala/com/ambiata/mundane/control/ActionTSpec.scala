package com.ambiata.mundane
package control

import testing.ResultMatcher._
import testing.Arbitraries._
import testing.Laws._
import org.specs2._, specification._, matcher._
import scalaz._, Scalaz._, \&/._
import scalaz.effect._

class ActionTSpec extends Specification with ScalaCheck with ThrownExpectations { def is = s2"""

 ActionT Examples
 ================

   logging                                    $logging
   creation of action from an IO[Result[A]]   $fromIOResult

"""

  type Logger = String => IO[Unit]
  type ExampleIOAction[A] = ActionT[IO, Unit, Logger, A]
  object ExampleIOAction extends ActionTSupport[IO, Unit, Logger]

  def logging = {

    def log(s: String): ExampleIOAction[Unit] =
      ExampleIOAction.ask.flatMap(logger =>
        logger(s).liftIO[ExampleIOAction])

    val action = for {
      value <- ExampleIOAction.ok("hello")
      _     <- log(value)
    } yield value

    val state = scala.collection.mutable.ListBuffer[String]()
    val logger = (s: String) => IO { state += s; () }

    action.execute(logger).unsafePerformIO must beOkValue("hello")
    state.toList  must_== List("hello")
  }

  def fromIOResult = {
    val logger = (s: String) => IO(())
    ExampleIOAction.fromIOResult(IO(Result.ok("hello"))).execute(logger).unsafePerformIO must beOkValue("hello")
  }


}
