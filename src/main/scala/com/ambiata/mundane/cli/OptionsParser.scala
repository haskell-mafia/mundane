package com.ambiata.mundane.cli

import scalaz._,Scalaz._
import scala.util.Try
import scopt.Read
import java.util.Locale
import org.joda.time.LocalDate

/**
 * Enhanced Options parser returning a disjunction of String \/ T
 */
abstract class OptionsParser[T](name: String = "") extends scopt.OptionParser[T](name) with Reads {

  private val messages = new scala.collection.mutable.ListBuffer[String]

  override def reportWarning(message: String) {
    messages += "WARN:  "+message
    ()
  }

  override def reportError(message: String) {
    messages += "ERROR: "+message
    ()
  }

  def parse(args: Array[String], init: T): String \/ T =
    Try(super.parse(args.toSeq, init)).toOption.flatten
      .fold(messages.mkString("\n").left[T])(_.right[String])
}

trait Reads {
  implicit def localDateRead: Read[LocalDate] = Read.reads { s: String =>
    new LocalDate(Read.calendarRead("yyyy-MM-dd", Locale.getDefault).reads(s))
  }
}

object Reads extends Reads
