package com.ambiata.mundane.testing

import org.specs2._, specification._, matcher._
import scalaz._, Scalaz._

class PathsSpec extends Specification with ScalaCheck { def is = s2"""

 Paths Arbitrary Properties
 ===========================

 only alpha, numeric, hyphen and slash            $chars
 no empty path components                         $noEmpties
 at least one path                                $nonEmpty
 not too many                                     $notTooMany

"""

  def chars = prop((paths: Keys) =>
    paths.keys.forall(_.path.matches("""[a-zA-Z/-]+""")))

  def noEmpties = prop((paths: Keys) =>
    !paths.keys.exists(_.path.matches(""".*//.*""")))

  def nonEmpty = prop((paths: Keys) =>
    !paths.keys.isEmpty)

  def notTooMany = prop((paths: Keys) =>
    paths.keys.size < 1000)
}
