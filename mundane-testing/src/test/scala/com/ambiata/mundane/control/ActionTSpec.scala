package com.ambiata.mundane
package control

import testing.ResultMatcher._
import org.specs2._, matcher._
import scalaz._, Scalaz._
import scalaz.effect._

class ActionTSpec extends Specification with ScalaCheck with ThrownExpectations { def is = s2"""

 ActionT Examples
 ================

   logging                                    $logging
   creation of action from an IO[Result[A]]   $fromIOResult
   using handles ok                           $usingOk
   using handles failure                      $usingFailure

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

  def usingOk = {
    var closed = 0
    implicit def x = new Resource[Int] {
      def close(i: Int) = IO { closed += 1 }
    }
    ActionT.using[Int, Int, Unit, Unit, Int](ActionT.ok[IO, Unit, Unit, Int](0))((i: Int) =>
      ActionT.ok[IO, Unit, Unit, Int](i + 1)).execute(Unit).unsafePerformIO() must beOkValue(1) and(closed must_== 1)
  }

  def usingFailure = {
    var closed = 0
    implicit def x = new Resource[Int] {
      def close(i: Int) = IO { closed += 1 }
    }
    ActionT.using[Int, Int, Unit, Unit, Int](ActionT.ok[IO, Unit, Unit, Int](0))((i: Int) =>
      ActionT.fail[IO, Unit, Unit, Int]("fail")).execute(Unit).unsafePerformIO() must beFail and(closed must_== 1)
  }

}
