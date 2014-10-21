package com.ambiata.mundane.parse

import scalaz._, Scalaz._
import org.joda.time._
import format.DateTimeFormat
import scalaz.Failure
import scala.Some
import scalaz.Success
import ListParser._
import scalaz.Validation.FlatMap._

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
          case head :: rest => (input.mkString(", ") + "\n" + f + s" (position: $i)").failure
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

  def named(n: String): ListParser[A] =
    ListParser((position, state) =>
      parse(position, state) match {
        case Failure((p, message)) => Failure((p, message+s" (for $n)"))
        case success               => success
      })

  def nonempty(implicit ev: A =:= String) =
    flatMap(a => ListParser((position, state) =>
      if (ev(a).isEmpty) (position, s"Expected string at position $position to be non empty").failure
      else (position, state, a).success
    ))

  def oflength(len: Int)(implicit ev: A =:= String) =
    flatMap(a => ListParser((position, state) =>
      if (ev(a).length != len) (position, s"Expected string at position $position to be of length $len").failure
      else (position, state, a).success
    ))

  def oflength(from: Int, to: Int)(implicit ev: A =:= String) =
    flatMap(a => ListParser((position, state) =>
      if (ev(a).length < from || ev(a).length > to) (position, s"Expected string at position $position to be of length between $from and $to").failure
      else (position, state, a).success
    ))

  def oflengthifsome(len: Int)(implicit ev: A =:= Option[String]) =
    flatMap(a => ListParser((position, state) => ev(a) match {
      case None    => (position, state, None).success
      case Some(x) if (x.length == len) => (position, state, Some(x)).success
      case Some(x) => (position, s"Expected the optional string at position $position to be of length $len if it exists").failure
    }))

  def oflengthifsome(from: Int, to: Int)(implicit ev: A =:= Option[String]) =
    flatMap(a => ListParser((position, state) => ev(a) match {
      case None    => (position, state, None).success
      case Some(x) if (x.length >= from && x.length <= to) => (position, state, Some(x)).success
      case Some(x) => (position, s"Expected the optional string at position $position to be of length between $from and $to if it exists").failure
    }))

  def option: ListParser[Option[A]] =
    ListParser((position, state) => state match {
      case "" :: t => (position + 1, t, None).success
      case xs => parse(position, xs).map(_.map(Option.apply[A]))
    })

  def commaDelimited(delimiter: Char = ','): ListParser[Seq[A]] =
    delimited(',')

  def delimited(delimiter: Char): ListParser[Seq[A]] =
    ListParser.delimitedValues(this, delimiter)

  def bracketed(opening: Char, closing: Char): ListParser[A] =
    ListParser.bracketed(this, opening, closing)

  def |||(x: ListParser[A]): ListParser[A] =
    ListParser((n, ls) =>
      parse(n, ls) match {
        case s @ Success(_) => s
        case Failure(_)     => x.parse(n, ls)
      })

  def * : ListParser[List[A]] =
    this.+ ||| success(Nil)

  def + : ListParser[List[A]] = for {
    x <- this
    xs <- this *
  } yield x :: xs
}

/**
 * Standard List parsers
 */
object ListParser {
  type ParseResult[A] = Validation[(Int, String), (Int, List[String], A)]

  /** The parser that always succeeds with the specified value. */
  def success[A](a: A): ListParser[A] =
    ListParser((position, state) => (position, state, a).success)

  /** The parser that always fails. */
  def fail[A](message: String): ListParser[A] =
    ListParser((position, state) => (position, message).failure)

  /**
   * a parser returning the current position (1-based) but does not consume any input
   * If the input has no elements the position is 0
   */
  def getPosition: ListParser[Int] =
    ListParser((position, state) => (position, state, position).success)

  /** A convenience function for cunstructoring parsers from scalaz style parseX functions. */
  def parseWithType[E, A](p: String => Validation[E, A], annotation: String): ListParser[A] =
    string.flatMap(s => value(p(s).leftMap(_ => s"""$annotation: '$s'""")))

  /** A convenience function for custom string parsers */
  def parseAttempt[A](p: String => Option[A], annotation: String): ListParser[A] =
    parseWithType(s => p(s).toSuccess(()), annotation)

  /** A byte, parsed accoding to java.lang.Byte.parseByte */
  def byte: ListParser[Byte] =
    parseWithType(_.parseByte, "not a byte")

  /** A short, parsed accoding to java.lang.Short.parseShort */
  def short: ListParser[Short] =
    parseWithType(_.parseShort, "not a short")

  /** An int, parsed accoding to java.lang.Integer.parseInt */
  def int: ListParser[Int] =
    parseWithType(_.parseInt, "not an int")

  /** A long, parsed accoding to java.lang.Long.parseLong */
  def long: ListParser[Long] =
    parseWithType(_.parseLong, "not a long")

  /** A double, parsed accoding to java.lang.Double.parseDouble */
  def double: ListParser[Double] =
    parseWithType(_.parseDouble, "not a double")

  /** A boolean, parsed accoding to java.lang.Boolean.parseBoolean */
  def boolean: ListParser[Boolean] =
    parseWithType(_.parseBoolean, "not a boolean")

  /** A char, the head of a single character string */
  def char: ListParser[Char] =
    parseAttempt(s => s.headOption.filter(_ => s.length == 1), "Not a char")

  /** Exactly one token, can only fail if the input is empty. */
  def string: ListParser[String] =
    ListParser((pos, str) => str match {
      case h :: t => (pos + 1, t, h).success
      case Nil    => (pos, s"not enough input, expected more than $pos fields.").failure
    })

  /** Possibly one token, or [[None]] if exhausted */
  def stringOpt: ListParser[Option[String]] =
    string.map(_.some) ||| none.pure[ListParser]

  def debug(tag: String): ListParser[Unit] =
    ListParser((position, state) => {
      println(s"[$tag] ${position}, ${state}")
      (position, state, ()).success
    } )

  /**
   * A parser for a local date with a given format, where format means joda time
   * supported formats: http://joda-time.sourceforge.net/apidocs/org/joda/time/format/DateTimeFormat.html
   */
  def localDateFormat(format: String): ListParser[LocalDate] =
    string.flatMap(s => valueOr(DateTimeFormat.forPattern(format).parseLocalDate(s),
                                _ => s"""not a local date with format $format: '$s'"""))

  /**
   * A parser for a local date-time with a given format, where format means joda time
   * supported formats: http://joda-time.sourceforge.net/apidocs/org/joda/time/format/DateTimeFormat.html
   */
  def localDatetimeFormat(format: String): ListParser[LocalDateTime] =
    string.flatMap(s => valueOr(DateTimeFormat.forPattern(format).parseLocalDateTime(s),
                                _ => s"""not a local date time with format $format: '$s'"""))

  /**
   * A parser for a local date with the `yyyy-MM-dd` format.
   */
  def localDate: ListParser[LocalDate] =
    localDateFormat("yyyy-MM-dd")

  /**
   * A parser for a local date with the `dd-MM-yyyy HH:mm:ss` format
   */
  def localDateTime: ListParser[LocalDateTime] =
    localDatetimeFormat("yyyy-MM-dd HH:mm:ss")

  /**
   * A parser for a value of type A
   */
  def value[A](f: => Validation[String, A]): ListParser[A] =
    ListParser((position, str) => f.bimap((position,_), (position, str, _)))

  /**
   * A parser for a value of type A with a failure message in case of an exception
   */
  def valueOr[A](a: => A, failure: Throwable => String): ListParser[A] =
    value(Validation.fromTryCatchNonFatal(a).leftMap(failure))

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

  /**
   * A parser for a pair of 2 delimited values
   */
  def pair[A, B](pa: ListParser[A], pb: ListParser[B], delimiter: Char): ListParser[(A, B)] =
    ListParser((position, state) => state match {
      case h :: t   =>
        pa.parse(Delimited.parseRow(h, delimiter)).flatMap {
          case (_, as, a) => pb.parse(as).map { case (_, _, b) => (position + h.size, t, (a, b)) }
        }

      case Nil    => (position, s"Expected string at position $position to be non empty").failure
    })

  /**
   * A parser for a list delimited values of the same type
   */
  def delimitedValues[A](p: ListParser[A], delimiter: Char): ListParser[Seq[A]] =
    ListParser((position, state) => state match {
      case h :: t =>
        val parsed = repeat(p).parse(Delimited.parseRow(h, delimiter))
        parsed.map { case (_, _, as) => (position + h.size, t, as) }

      case Nil => (position, Nil, Nil).success
    })

  /**
   * A parser that repeats the application of another parser until all the input is consumed
   */
  def repeat[A](p: ListParser[A]): ListParser[Seq[A]] =
    emptySeq ||| p.flatMap(a => repeat(p).map(seq => a +: seq))

  /**
   * A parser that succeeds on an empty list and returns an empty sequence
   */
  def emptySeq[A]: ListParser[Seq[A]] =
    ListParser((position, state) =>
      if (state.isEmpty) (position, state, Nil).success[(Int, String)]
      else               (position, s"$state is not an empty list").failure[(Int, List[String], Seq[A])]
    )

  /**
   * A parser for a value that is surrounded by 2 other characters
   */
  def bracketed[A](parser: ListParser[A], opening: Char, closing: Char): ListParser[A] =
    for {
      _ <- value(opening.success)
      a <- parser
      _ <- value(closing.success)
    } yield a

  /**
   * A parser for key value maps
   */
  def keyValueMap[K, V](key: ListParser[K], value: ListParser[V], entriesDelimiter: Char, keyValueDelimiter: Char): ListParser[Map[K, V]] =
    delimitedValues(pair[K, V](key, value, keyValueDelimiter), entriesDelimiter).map(_.toMap)

  /**
   * A parser for key value maps with json delimiters
   */
  def jsonKeyValueMap[K, V](key: ListParser[K], value: ListParser[V]): ListParser[Map[K, V]] =
    keyValueMap(key, value, ',', ':')

  implicit def ListParserMonad: Monad[ListParser] = new Monad[ListParser] {
    def bind[A, B](r: ListParser[A])(f: A => ListParser[B]) = r flatMap f
    def point[A](a: => A) = value(a.success)
  }
}
