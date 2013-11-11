package com.ambiata.mundane.parse

import scalaz._, Scalaz._
import org.joda.time._
import format.DateTimeFormat

/**
 * Parser for a list of strings, returning a Failure[String] if the parse failed, or an object A
 */
case class ListParser[A](parse: (Int, List[String]) => Validation[String, (Int, List[String], A)]) {
  def run(input: List[String]): Validation[String, A] =
    parse(0, input).flatMap {
      case (_, Nil, a) => a.success
      case (_, x, _)   => s"There was more input not consumed: $x".failure
    }

  def map[B](f: A => B): ListParser[B] =
    flatMap(a => ListParser.value(f(a).success))

  def flatMap[B](f: A => ListParser[B]): ListParser[B] =
    ListParser((position, state) =>
      parse(position, state) match {
        case Success((nuposition, nustate, a)) => f(a).parse(nuposition, nustate)
        case Failure(error)                    => Failure(error)
      })
}

/**
 * Standard List parsers
 */
object ListParser {
  /**
   * a parser returning the current position (1-based) but does not consume any input
   * If the input has no elements the position is 0
   */
  def getPosition: ListParser[Int] =
    ListParser((position, state) => (position, state, position).success)

  /**
   * A parser for an Int
   */
  def int: ListParser[Int] = for {
    s         <- string
    position  <- getPosition
    result    <- value(s.parseInt.leftMap(_ => s"""Not an int at position $position: '$s'"""))
  } yield result

  /**
   * A parser for a local date with a given format
   */
  def localDate(format: String): ListParser[LocalDate] = for {
    s        <- string
    position <- getPosition
    result   <- value(DateTimeFormat.forPattern(format).parseLocalDate(s),
                { t => s"""Not a local date with format $format at position $position: '$s'""" })
  } yield result

  /**
   * A parser for a local date with the dd/MM/yyyy format
   */
  def localDate: ListParser[LocalDate] = localDate("dd/MM/yyyy")

  /**
   * A parser for a Double
   */
  def double: ListParser[Double] = for {
    s         <- string
    position  <- getPosition
    result    <- value(s.parseDouble.leftMap(_ => s"""Not a double at position $position: '$s'"""))
  } yield result

  /**
   * A parser for a String
   */
  def string: ListParser[String] =
    ListParser((pos, str) => str match {
      case h :: t => (pos + 1, t, h).success
      case Nil    => s"Not enough input, expected more than $pos fields.".failure
    })

  /**
   * A parser for a value of type A
   */
  def value[A](f: =>Validation[String, A]): ListParser[A] =
    ListParser((position, str) => f.map((position, str, _)))

  /**
   * A parser for a value of type A with a failure message in case of an exception
   */
  def value[A](a: =>A, failure: Throwable => String): ListParser[A] =
    value(Validation.fromTryCatch(a).leftMap(failure))

  /**
   * A parser consuming n positions in the input
   */
  def consume(n: Int): ListParser[Unit] =
    ListParser((pos, str) => {
      val nupos = pos + n
      str.length match {
        case l if n <= l => (nupos, str.slice(n, l), ()).success
        case _           => s"Not enough input, expected more then $nupos.".failure
      }
    })

  /**
   * A parser consuming all remaining fields
   */
  def consumeRest: ListParser[Unit] =
    ListParser((pos, str) => (str.length, Nil, ()).success)

  implicit def ListParserMonad: Monad[ListParser] = new Monad[ListParser] {
    def bind[A, B](r: ListParser[A])(f: A => ListParser[B]) = r flatMap f
    def point[A](a: => A) = value(a.success)
  }
}
