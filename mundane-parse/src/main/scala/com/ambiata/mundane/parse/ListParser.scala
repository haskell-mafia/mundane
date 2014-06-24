package com.ambiata.mundane.parse

import scalaz._, Scalaz._
import org.joda.time._
import format.DateTimeFormat
import scalaz.Failure
import scala.Some
import scalaz.Success
import ListParser._

/**
 * Parser for a list of strings, returning a Failure[String] if the parse failed, or an object A
 */
case class ListParser[A](parse: (Int, List[String]) => ParseResult[A]) {
  def parse(input: List[String]): ParseResult[A] =
    parse(0, input)

  def run(input: List[String]): Validation[String, A] =
    parse(input) match {
      case Success(s) =>
        s match {
          case (_, Nil, a) => a.success
          case (p, x, _)   =>
            s"""|Parsed successfully: $input up to position $p
                | -> but the rest of the list was not consumed: $x""".stripMargin.failure

        }

      case Failure((i, f)) =>
        input match {
          case Nil => Failure(f)
          case head :: rest =>
            val caretPosition = input.take(i - 1).map(_.size + 2).sum + input(i - 1).size / 2
            val messagePosition = scala.math.max(0, caretPosition - (f.size / 2))
            val finalMessage =
              input.mkString(", ") + "\n" +
              (" " * caretPosition) + "^\n" +
              (" " * messagePosition) + f + s" (position: $i)"

            finalMessage.failure
        }

    }

  def preprocess(f: String => String): ListParser[A] =
    ListParser[A]((i: Int, list: List[String]) => parse(i, list.map(f)))

  def map[B](f: A => B): ListParser[B] =
    flatMap(a => ListParser.value(f(a).success))

  def flatMap[B](f: A => ListParser[B]): ListParser[B] =
    ListParser((position, state) =>
      parse(position, state) match {
        case Success((nuposition, nustate, a)) => f(a).parse(nuposition, nustate)
        case Failure(error)                    => Failure(error)
      })

  def nonempty(implicit ev: A =:= String) =
    flatMap(a => ListParser((position, state) =>
      if (ev(a).isEmpty) (position, s"Expected string at position $position to be non empty").failure
      else (position, state, a).success
    ))

  def option: ListParser[Option[A]] =
    ListParser((position, state) => state match {
      case "" :: t => (position + 1, t, None).success
      case xs => parse(position, xs).map(_.map(Option.apply[A]))
    })

  def delimited(implicit ev: A =:= String, delimiter: Char=','): ListParser[Seq[String]] =
    flatMap(a => ListParser((position, state) =>
      if (ev(a).isEmpty) (position, state, Seq()).success
      else               (position, state, Delimited.parseRow(a, delimiter)).success
    ))

  def |||(x: ListParser[A]): ListParser[A] =
    ListParser((n, ls) =>
      x.parse(n, ls) match {
        case s @ Success(_) => s
        case Failure(_)     => x.parse(n, ls)
      })
}

/**
 * Standard List parsers
 */
object ListParser {
  type ParseResult[A] = Validation[(Int, String), (Int, List[String], A)]

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
    result    <- value(s.parseInt.leftMap(_ => s"""not an int: '$s'"""))
  } yield result

  /**
   * A parser for a Short
   */
  def short: ListParser[Short] = for {
    s         <- string
    position  <- getPosition
    result    <- value(s.parseShort.leftMap(_ => s"""not a short: '$s'"""))
  } yield result

  /**
   * A parser for a local date with a given format
   */
  def localDate(format: String): ListParser[LocalDate] = for {
    s        <- string
    position <- getPosition
    result   <- value(DateTimeFormat.forPattern(format).parseLocalDate(s),
                      { t => s"""not a local date with format $format: '$s'""" })
  } yield result

  /**
   * A parser for a local date with the yyyy-MM-dd format
   */
  def localDate: ListParser[LocalDate] = localDate("yyyy-MM-dd")

  /**
   * A parser for a local date with a given format
   */
  def localDatetime(format: String): ListParser[LocalDateTime] = for {
    s        <- string
    position <- getPosition
    result   <- value(DateTimeFormat.forPattern(format).parseLocalDateTime(s),
                      { t => s"""not a local date time with format $format: '$s'""" })
  } yield result

  /**
   * A parser for a local date with the dd/MM/yyyy format
   */
  def localDateTime: ListParser[LocalDateTime] = localDatetime("yyyy-MM-dd HH:mm:ss")

  /**
   * A parser for a Double
   */
  def double: ListParser[Double] = for {
    s         <- string
    position  <- getPosition
    result    <- value(s.parseDouble.leftMap(_ => s"""not a double: '$s'"""))
  } yield result

  /**
   * A parser for a Boolean
   */
  def boolean: ListParser[Boolean] = for {
    s         <- string
    position  <- getPosition
    result    <- value(s.parseBoolean.leftMap(_ => s"""not a boolean: '$s'"""))
  } yield result

  /**
   * A parser for a String
   */
  def string: ListParser[String] =
    ListParser((pos, str) => str match {
      case h :: t => (pos + 1, t, h).success
      case Nil    => (pos, s"not enough input, expected more than $pos fields.").failure
    })

  /**
   * A parser for a value of type A
   */
  def value[A](f: =>Validation[String, A]): ListParser[A] =
    ListParser((position, str) => f.bimap((position,_), (position, str, _)))

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
        case _           => (nupos, s"not enough input, expected more than $nupos.").failure
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
