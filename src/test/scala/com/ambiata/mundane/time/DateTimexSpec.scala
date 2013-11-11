package com.ambiata.mundane
package time

import scalaz._,Scalaz._
import org.specs2._
import time.NoTimeConversions
import matcher._
import org.joda.time._

class DateTimexSpec extends Specification with NoTimeConversions with DateTimex { def is = s2"""

 Given an interval, we can access all the days in that interval
 ${ fromNowTo(5.days).toLocalDates.list must haveSize(5) }

"""
}
