package com.ambiata.mundane.time

import scalaz._, Scalaz._
import NonEmptyList._
import org.joda.time._

trait DateTimex {

  /** Order instance for DateTime */
  implicit def DateTimeHasOrder: Order[DateTime] = Order.fromScalaOrdering[DateTime]

  implicit def DateTimeHasOrdering: scala.Ordering[DateTime] = new scala.Ordering[DateTime] {
    def compare(d1: DateTime, d2: DateTime) = d1.compareTo(d2)
  }

  /** Order instance for LocalDate */
  implicit def LocalDateHasOrder: Order[LocalDate] = Order.fromScalaOrdering[LocalDate]

  implicit def LocalDateHasOrdering: scala.Ordering[LocalDate] = new scala.Ordering[LocalDate] {
    def compare(d1: LocalDate, d2: LocalDate) = d1.compareTo(d2)
  }

  /** Order instances for Interval */
  implicit def IntervalHasOrder: Order[Interval] = Order.fromScalaOrdering[Interval]

  implicit def IntervalHasOrdering: scala.Ordering[Interval] = new scala.Ordering[Interval] {
    def compare(i1: Interval, i2: Interval) =
      if (i1.getEnd < i2.getStart)      -1
      else if (i2.getEnd < i1.getStart) 1
      else                              0
  }

  /** Order instances for Period */
  implicit def PeriodHasOrder: Order[Period] = Order.fromScalaOrdering[Period]

  implicit def PeriodHasOrdering: scala.Ordering[Period] = new scala.Ordering[Period] {
    def compare(i1: Period, i2: Period) = i1.getMillis.compareTo(i2.getMillis)
  }

  /**
   * return a list of all the days in a given interval
   */
  implicit class IntervalToLocalDates(interval: Interval) {
    def toLocalDates: NonEmptyList[LocalDate] = {
      val rest =
      // we need to make sure that start + 1 day is before end otherwise 'withStart' returns null
        if (tomorrow.isBefore(end)) interval.withStart(tomorrow).toLocalDates.list
        else                        Nil
      nel(start.toLocalDate, rest)
    }

    private def tomorrow = start.plusDays(1)
    private def start = interval.getStart
    private def end   = interval.getEnd
  }

  implicit class IntToPeriod(i: Int) {
    def second  = i.seconds
    def seconds = Period.seconds(i)
    def minute  = i.minutes
    def minutes = Period.minutes(i)
    def hour    = i.hours
    def hours   = Period.hours(i)
    def day     = i.days
    def days    = Period.days(i)
    def week    = i.weeks
    def weeks   = Period.weeks(i)
    def year    = i.years
    def years   = Period.years(i)
  }

  /** create an interval starting from now and lasting a given period (of days, weeks, years...) */
  def fromNowTo(period: Period): Interval = {
    val start = DateTime.now.toLocalDate.toDateTimeAtStartOfDay
    new Interval(start, start.plusDays(period.getDays))
  }

}
