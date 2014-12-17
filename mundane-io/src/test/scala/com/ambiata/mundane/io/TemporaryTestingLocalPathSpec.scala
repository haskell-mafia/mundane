package com.ambiata.mundane.io

import com.ambiata.mundane.control.RIO
import com.ambiata.mundane.io.{TemporaryTestingLocalPath => TT}
import com.ambiata.mundane.testing.RIOMatcher._

import org.specs2._
import scalaz.{Failure => _, _}, Scalaz._

class TemporaryTestingLocalPathSpec extends Specification  { def is = s2"""

   ${ TT.withLocalPath(path => (path ==== path).pure[RIO] ) }

   ${ TT.withLocalPath(path => path.mkdirs >> switchResult(path.determineFile) >> ok.pure[RIO] )  }

   ${ TT.withLocalPath(path => path.touch >> switchResult(path.determineDirectory) >> ok.pure[RIO] ) }
"""
}
