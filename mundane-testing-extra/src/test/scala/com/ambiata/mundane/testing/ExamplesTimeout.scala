package com.ambiata.mundane.testing

import org.specs2.execute._
import org.specs2.main._
import org.specs2.matcher._
import org.specs2.specification._
import org.specs2.time._

/**
 * This trait can be used to add a global time out to each example or for a specific one:
 *
 *  - for each example mix-in the trait
 *  - for a single example import the object and use the upTo context:
 *
 *   my example must terminate in a reasonable amount of time ${upTo(3.seconds)(e1)}
 */
trait ExamplesTimeout extends AroundExample with MustMatchers with TerminationMatchers with CommandLineArguments with TimeConversions {

  lazy val commandLineTimeOut = arguments.commandLine.int("timeout").map(_.millis)

  lazy val defaultTimeOut = 1.minute

  def timeout = commandLineTimeOut.getOrElse(defaultTimeOut)

  def around[T : AsResult](t: =>T) = upTo(timeout).around(t)

  def upTo(to: Duration) = new Around {
    def around[T : AsResult](t: =>T) = {
      lazy val result = t
      val termination = result must terminate(retries = 10, sleep = (to.inMillis / 10).millis).orSkip((ko: String) => "TIMEOUT: "+to)

      if (!termination.toResult.isSkipped) AsResult(result)
      else termination.toResult
    }
  }

}

object ExamplesTimeout extends ExamplesTimeout
