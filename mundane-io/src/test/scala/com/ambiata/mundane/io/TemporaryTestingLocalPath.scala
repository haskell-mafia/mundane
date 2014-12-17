package com.ambiata.mundane.io

import com.ambiata.mundane.control._
import com.ambiata.mundane.path._
import com.ambiata.mundane.io.Temporary._
import com.ambiata.mundane.io.TemporaryLocalPath._
import com.ambiata.mundane.testing.RIOMatcher._
import org.specs2._, matcher._, execute.{Result => SpecsResult, Error => SpecsError, _}
import scalaz.{Success => _, Failure => _, _}, Scalaz._, effect.IO, \&/._

object TemporaryTestingLocalPath extends ScalaCheckMatchers {
  def withLocalPathR[A](f: LocalPath => RIO[MatchResult[A]]): RIO[MatchResult[A]] = {
    runWithLocalPath(LocalPath(uniqueLocalPath))(f)
  }

  def withLocalPath[A](f: LocalPath => RIO[MatchResult[A]]): SpecsResult = {
    rioToSpecsResult(runWithLocalPath(LocalPath(uniqueLocalPath))(x => f(x)))
  }

  def rioToSpecsResult[A: AsResult](t: RIO[A]): SpecsResult =
    t.run.unsafePerformIO match {
      case Ok(actual)     => AsResult(actual)
      case Error(error)   => Failure(s"Result failed with <${Result.asString(error)}>")
    }
}
