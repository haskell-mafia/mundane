package com.ambiata.mundane
package time

import org.specs2.Specification
import org.joda.time.{Interval, DateTime, LocalDate}
import org.specs2.matcher.ThrownExpectations
import org.specs2.specification.Tables

class TimedPathsSpec extends Specification with TimedPaths with ThrownExpectations with Tables { def is = s2"""

 TimedPaths encodes and decodes paths where a creation time is specified
   localDateToPath    $e1
   dateTimeToPath     $e2
   pathToDateTime     $e3
   pathToLocalDate    $e4
   intervalToPaths    $e5
                      """

  def e1 = localDateToPath(new LocalDate(2013, 11, 2))                   === "year=2013/month=11/day=02"
  def e2 = dateTimeToPath(new DateTime(2013, 3, 20, 15, 12, 11))         === "year=2013/month=03/day=20/hour=15"
  def e3 = {
    pathToDateTime("/year=2013/month=11/day=20/hour=15/part.psv")     === Some(new DateTime(2013, 11, 20, 15, 0, 0))
    pathToDateTime("any/year=2013/month=11/day=20/hour=15/part.psv")  === Some(new DateTime(2013, 11, 20, 15, 0, 0))
  }
  def e4 = {
    pathToLocalDate("/year=2013/month=11/day=20/hour=15/part.psv")    === Some(new LocalDate(2013, 11, 20))
    pathToLocalDate("any/year=2013/month=11/day=20/hour=15/part.psv") === Some(new LocalDate(2013, 11, 20))
  }

  def e5 = {
    "start"      | "end"         | "paths"                                                                                                                |>
      "2014-03-10" !  "2014-03-10" ! ".*year=2014/month=03/day=10.*"                                                                                        |
      "2014-01-01" !  "2014-12-31" ! ".*year=2014/month=.*/day=.*"                                                                                          |
      "2014-03-01" !  "2014-03-31" ! ".*year=2014/month=03/day=.*"                                                                                          |
      "2014-03-10" !  "2014-03-11" ! ".*year=2014/month=03/day=10.*, .*year=2014/month=03/day=11.*"                                                         |
      "2013-01-01" !  "2014-03-01" ! ".*year=2013/month=.*/day=.*, .*year=2014/month=01/day=.*, .*year=2014/month=02/day=.*, .*year=2014/month=03/day=01.*" |
      { (start, end, paths) =>
        intervalToPaths(new LocalDate(start), new LocalDate(end)) must_== paths.split(",").toList.map(_.trim)
      }
  }

}
