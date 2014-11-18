package com.ambiata.mundane.parse

import scalaz._, Scalaz._
import org.joda.time._
import format.DateTimeFormat
import scalaz.Failure
import scala.Some
import scalaz.Success
import DelimitedParser._
import scalaz.Validation.FlatMap._

case class DelimitedParserState(position: Int, input: String, delim: Char) {

  def getAndNext: (Option[String], DelimitedParserState) =
    if (!hasMore) (None, this)
    else {
      val s = get
      (s.some, copy(position = Math.min(position + s.length + 1, input.length)))
    }

  def get: String = {
    val next = input.indexOf(delim, position)
    // This is relying on Java 6 substring - we need our own, faster implementation of String to do in a nicer way
    input.substring(position, if (next < 0) input.length else next)
  }

  def inc(i: Int): Option[DelimitedParserState] = {
    var j = i
    var pos = position
    while (j > 0 && pos < input.length) {
      pos = input.indexOf(delim, pos)
      pos = if (pos >= 0) pos + 1 else input.length
      j -= 1
    }
    if (j == 0) Some(copy(position = pos))
    else None
  }

  def rest: String =
    input.substring(position)

  def hasMore: Boolean =
    position < input.length
}

/**
 * Parser for a list of strings, returning a Failure[String] if the parse failed, or an object A
 */
case class DelimitedParser[A](run: DelimitedParserState => ParseResult[A]) {
  def parse(input: String, delim: Char): ParseResult[A] =
    run(DelimitedParserState(0, input, delim))

  def run(input: String, delim: Char): Validation[String, A] =
    parse(input, delim) match {
      case Success((state, a)) =>
        if (!state.hasMore) a.success
        else
          s"""|Parsed successfully: ${state.input} up to position ${state.position}
              | -> but the rest of the list was not consumed: ${state.rest}""".stripMargin.failure

      case Failure((s, f)) =>
        (input + "\n" + f + s" (position: ${s.position})").failure

    }

  def preprocess(f: String => String): DelimitedParser[A] =
    DelimitedParser[A](state => ???)

  def map[B](f: A => B): DelimitedParser[B] =
    flatMap(a => DelimitedParser.value(f(a).success))

  def flatMap[B](f: A => DelimitedParser[B]): DelimitedParser[B] =
    DelimitedParser(state =>
      run(state) match {
        case Success((state2, a)) => f(a).run(state2)
        case Failure(error)       => Failure(error)
      })

  def named(n: String): DelimitedParser[A] =
    DelimitedParser(state =>
      run(state) match {
        case Failure((p, message)) => Failure((p, s"$message (for $n)"))
        case success               => success
      })

  def whenEmpty(a: A): DelimitedParser[A] =
    DelimitedParser.empty(a) ||| this

  def nonempty(implicit ev: A =:= String): DelimitedParser[String] =
    flatMap(a => DelimitedParser(state =>
      if (ev(a).isEmpty) (state, s"Expected string at position ${state.position} to be non empty").failure
      else (state, ev(a)).success
    ))

  def oflength(len: Int)(implicit ev: A =:= String): DelimitedParser[String] =
    flatMap(a => DelimitedParser(state =>
      if (ev(a).length != len) (state, s"Expected string at position ${state.position} to be of length $len").failure
      else (state, ev(a)).success
    ))

  def oflength(from: Int, to: Int)(implicit ev: A =:= String): DelimitedParser[String] =
    flatMap(a => DelimitedParser(state =>
      if (ev(a).length < from || ev(a).length > to) (state, s"Expected string at position ${state.position} to be of length between $from and $to").failure
      else (state, ev(a)).success
    ))

  def oflengthifsome(len: Int)(implicit ev: A =:= Option[String]): DelimitedParser[Option[String]] =
    flatMap(a => DelimitedParser(state => ev(a) match {
      case None    => (state, None).success
      case Some(x) if x.length == len => (state, Some(x)).success
      case Some(x) => (state, s"Expected the optional string at position ${state.position} to be of length $len if it exists").failure
    }))

  def oflengthifsome(from: Int, to: Int)(implicit ev: A =:= Option[String]): DelimitedParser[Option[String]] =
    flatMap(a => DelimitedParser(state => ev(a) match {
      case None    => (state, None).success
      case Some(x) if x.length >= from && x.length <= to => (state, Some(x)).success
      case Some(x) => (state, s"Expected the optional string at position ${state.position} to be of length between $from and $to if it exists").failure
    }))

  def option: DelimitedParser[Option[A]] =
    DelimitedParser(state => ???)

  def |||(x: DelimitedParser[A]): DelimitedParser[A] =
    DelimitedParser(state =>
      run(state) match {
        case s @ Success(_) => s
        case Failure(_)     => x.run(state)
      })

  def * : DelimitedParser[List[A]] =
    this.+ ||| success(Nil)

  def + : DelimitedParser[List[A]] = for {
    x <- this
    xs <- this *
  } yield x :: xs
}

/**
 * Standard List parsers
 */
object DelimitedParser {
  type ParseResult[A] = Validation[(DelimitedParserState, String), (DelimitedParserState, A)]

  /** The parser that always succeeds with the specified value. */
  def success[A](a: A): DelimitedParser[A] =
    DelimitedParser(state => (state, a).success)

  /** The parser that always fails. */
  def fail[A](message: String): DelimitedParser[A] =
    DelimitedParser(state => (state, message).failure)

  /**
   * a parser returning the current position (1-based) but does not consume any input
   * If the input has no elements the position is 0
   */
  def getPosition: DelimitedParser[Int] =
    DelimitedParser(state => (state, state.position).success)

  /** A convenience function for cunstructoring parsers from scalaz style parseX functions. */
  def parseWithType[E, A](p: String => Validation[E, A], annotation: String): DelimitedParser[A] =
    string.flatMap(s => value(p(s).leftMap(_ => s"""$annotation: '$s'""")))

  /** A convenience function for custom string parsers */
  def parseAttempt[A](p: String => Option[A], annotation: String): DelimitedParser[A] =
    parseWithType(s => p(s).toSuccess(()), annotation)

  /** A byte, parsed accoding to java.lang.Byte.parseByte */
  def byte: DelimitedParser[Byte] =
    parseWithType(_.parseByte, "not a byte")

  /** A short, parsed accoding to java.lang.Short.parseShort */
  def short: DelimitedParser[Short] =
    parseWithType(_.parseShort, "not a short")

  /** An int, parsed accoding to java.lang.Integer.parseInt */
  def int: DelimitedParser[Int] =
    parseWithType(_.parseInt, "not an int")

  /** A long, parsed accoding to java.lang.Long.parseLong */
  def long: DelimitedParser[Long] =
    parseWithType(_.parseLong, "not a long")

  /** A double, parsed accoding to java.lang.Double.parseDouble */
  def double: DelimitedParser[Double] =
    parseWithType(_.parseDouble, "not a double")

  /** A boolean, parsed accoding to java.lang.Boolean.parseBoolean */
  def boolean: DelimitedParser[Boolean] =
    parseWithType(_.parseBoolean, "not a boolean")

  /** A char, the head of a single character string */
  def char: DelimitedParser[Char] =
    parseAttempt(s => s.headOption.filter(_ => s.length == 1), "Not a char")

  /** Exactly one token, can only fail if the input is empty. */
  def string: DelimitedParser[String] =
    DelimitedParser(state => state.getAndNext match {
      case (Some(s), next) => (next, s).success
      case (None, next)    => (next, s"not enough input, expected more than ${next.position} fields.").failure
    })

  /** Possibly one token, or [[None]] if exhausted */
  def stringOpt: DelimitedParser[Option[String]] =
    string.map(_.some) ||| none.pure[DelimitedParser]

  def debug(tag: String): DelimitedParser[Unit] =
    DelimitedParser(state => {
      println(s"[$tag] ${state.position}")
      (state, ()).success
    } )

  /**
   * A parser for a local date with a given format, where format means joda time
   * supported formats: http://joda-time.sourceforge.net/apidocs/org/joda/time/format/DateTimeFormat.html
   */
  def localDateFormat(format: String): DelimitedParser[LocalDate] =
    string.flatMap(s => valueOr(DateTimeFormat.forPattern(format).parseLocalDate(s),
                                _ => s"""not a local date with format $format: '$s'"""))

  /**
   * A parser for a local date-time with a given format, where format means joda time
   * supported formats: http://joda-time.sourceforge.net/apidocs/org/joda/time/format/DateTimeFormat.html
   */
  def localDatetimeFormat(format: String): DelimitedParser[LocalDateTime] =
    string.flatMap(s => valueOr(DateTimeFormat.forPattern(format).parseLocalDateTime(s),
                                _ => s"""not a local date time with format $format: '$s'"""))

  /**
   * A parser for a local date with the `yyyy-MM-dd` format.
   */
  def localDate: DelimitedParser[LocalDate] =
    localDateFormat("yyyy-MM-dd")

  /**
   * A parser for a local date with the `dd-MM-yyyy HH:mm:ss` format
   */
  def localDateTime: DelimitedParser[LocalDateTime] =
    localDatetimeFormat("yyyy-MM-dd HH:mm:ss")

  /**
   * A parser for a value of type A
   */
  def value[A](f: => Validation[String, A]): DelimitedParser[A] =
    DelimitedParser(state => f.bimap((state, _), (state, _)))

  /**
   * A parser for a value of type A with a failure message in case of an exception
   */
  def valueOr[A](a: => A, failure: Throwable => String): DelimitedParser[A] =
    value(Validation.fromTryCatchNonFatal(a).leftMap(failure))

  /**
   * A parser consuming n positions in the input
   */
  def consume(n: Int): DelimitedParser[Unit] =
    DelimitedParser(state => state.inc(n).cata(
      (_, ()).success,
      (state, s"not enough input, expected more than ${state.input.length}.").failure
    ))

  /**
   * A parser consuming all remaining fields
   */
  def consumeRest: DelimitedParser[Unit] =
    DelimitedParser(state => (state.copy(position = state.input.length), ()).success)

  /**
   * A parser that repeats the application of another parser until all the input is consumed
   */
  def repeat[A](p: DelimitedParser[A]): DelimitedParser[List[A]] =
    emptyList ||| p.flatMap(a => repeat(p).map(seq => a +: seq))

  /**
   * A parser that succeeds on an empty list and returns a value instead
   */
  def empty[A](a: A): DelimitedParser[A] =
    DelimitedParser(state => {
      val s = state.get
      if (s.isEmpty) (state, a).success
      else (state, s"$s is not empty").failure
    })

  /**
   * A parser that succeeds on an empty list and returns an empty string
   */
  def emptyString: DelimitedParser[String] =
    empty("")

  /**
   * A parser that succeeds on an empty list and returns an empty sequence
   */
  def emptyList[A]: DelimitedParser[List[A]] =
    empty(Nil)

  /**
   * A parser that returns 0.0 on an empty list or parses a Double
   */
  def doubleOrZero: DelimitedParser[Double] =
    empty(0.0) ||| double

  /**
   * A parser that returns 0 on an empty list or parses an Int
   */
  def intOrZero: DelimitedParser[Int] =
    empty(0) ||| int



  implicit def DelimitedParserMonad: Monad[DelimitedParser] = new Monad[DelimitedParser] {
    def bind[A, B](r: DelimitedParser[A])(f: A => DelimitedParser[B]) = r flatMap f
    def point[A](a: => A) = value(a.success)
  }
}
