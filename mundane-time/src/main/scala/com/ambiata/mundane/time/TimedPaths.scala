package com.ambiata.mundane
package time

import org.joda.time.{DateTime, Interval, LocalDate}
import data.Regexes._
import DateTimex._

/**
 * This trait encodes time, read from a formatted string, into a path
 *
 * It can also read the part of a path where a date/time is expected and transform it back to a DateTime
 */
trait TimedPaths {

  /** @return a path for a given date */
  def localDateToPath(date: LocalDate) =
    s"year=${date.getYear}/month=${twoDigits(date.getMonthOfYear)}/day=${twoDigits(date.getDayOfMonth)}"

  /** @return a path for a given date */
  def dateTimeToPath(date: DateTime) =
    localDateToPath(date.toLocalDate)+s"/hour=${twoDigits(date.getHourOfDay)}"

  /** @return the time corresponding to a given path or None if the date can not be parsed */
  def pathToDateTime(path: String): Option[DateTime] = path match {
    case r"""(.*)${_}year=(\d+)$y/month=(\d+)$m/day=(\d+)$d/hour=(\d+)$h/.*""" => Some(new DateTime(y.toInt, m.toInt, d.toInt, h.toInt, 0))
    case _                                                                     => None
  }

  /** @return the time corresponding to a given path or None if the date can not be parsed */
  def pathToLocalDate(path: String): Option[LocalDate] = path match {
    case r"""(.*)${_}year=(\d+)$y/month=(\d+)$m/day=(\d+)$d/.*""" => Some(new LocalDate(y.toInt, m.toInt, d.toInt))
    case _                                                        => None
  }

  /** @return a list of patterns with stars covering exactly the interval (start and end date included) */
  def intervalToPaths(start: LocalDate, end: LocalDate): List[String] =
    intervalToPaths(new Interval(start.toDateTimeAtStartOfDay, end.plusDays(1).toDateTimeAtStartOfDay))

  /** @return a list of patterns with stars covering exactly the interval (start and end date included) */
  def intervalToPaths(interval: Interval): List[String] = {
    val byYear = interval.toLocalDates.list.groupBy(_.getYear)
    byYear.flatMap { case (year, dates) =>
      val numberOfDaysInYear = dates.head.dayOfYear.getMaximumValue
      if (dates.size == numberOfDaysInYear) Seq(s".*year=$year/month=.*/day=.*")
      else {
        val byMonth = dates.groupBy(_.getMonthOfYear)
        byMonth.flatMap { case (month, dates1) =>
          val numberOfDaysInMonth = dates1.head.dayOfMonth.getMaximumValue
          if (dates1.size == numberOfDaysInMonth) Seq(s".*year=$year/month=${twoDigits(month)}/day=.*")
          else dates1.map(day => s".*year=$year/month=${twoDigits(month)}/day=${twoDigits(day.getDayOfMonth)}.*")
        }
      }
    }.toList.sorted
  }

  private def twoDigits(value: Int) = {
    val result = value.toString
    if (result.size == 1) "0"+result
    else result
  }
}

object TimedPaths extends TimedPaths


