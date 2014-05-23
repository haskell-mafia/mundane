package com.ambiata.mundane.testing

import org.specs2.mutable.Specification

class ExamplesTimeoutSpec extends Specification with ExamplesTimeout {
  override lazy val defaultTimeOut = 200.millis

  "test the examples timeout" >> {
    Thread.sleep(3000)
    ok
  }

}
