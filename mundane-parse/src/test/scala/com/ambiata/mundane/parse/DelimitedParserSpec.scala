package com.ambiata.mundane.parse

import com.ambiata.mundane.parse.DelimitedParser._
import org.joda.time._
import org.scalacheck._
import org.specs2._
import org.specs2.matcher.{Matcher, ThrownExpectations, ValueCheck}

import scalaz.Scalaz._
import scalaz._

class DelimitedParserSpec extends Specification with ThrownExpectations with ScalaCheck { def is = s2"""

Examples
========

 Parsing a list will:
   extract the position of each element                                              $position1
   extract a string                                                                  $string1
   extract a string option                                                           $stringOpt1
   extract a string option from an empty list                                        $stringOpt2
   extract a nonempty string                                                         $nonemptystring1
   extract a value iff a string is empty                                             $emptyValue
   extract a string of a certain length                                              $lengthstring1
   extract an optional string of a certain length                                    $optionlengthstring1
   extract an int                                                                    $int1
   extract a short                                                                   $short1
   extract a byte                                                                    $byte1
   extract a char                                                                    $char1
   extract a double                                                                  $double1
   extract a LocalDate                                                               $localDate1
   consume elements                                                                  $consume1
   consume all remaining elements                                                    $consume2
   lift a value                                                                      $value1
   lift a validation                                                                 $validation1
   extract multiple values                                                           $multi1
   error if not parsed all elements                                                  $leftover1
   preprocess the input string                                                       $preprocess1
   option set                                                                        $option1
   option not set                                                                    $option2
   option invalid                                                                    $option3
   parsing lists                                                                     $intlist1
   parsing lists                                                                     $intlist2
   return a message containing the full list and the first failure if parsing fails  $fail1
   or combinator only executes other parser when there is a failure                  $orCombinator1
   or combinator never executes other parser when there is no failure                $orCombinator2
   or combinator fails when both first and second parsers fail                       $orCombinator3
   error if char is invalid                                                          $invalidChar1
   optionally provide a name in case of a failure                                    $named

Properties
==========

  byte is symmetric                               ${symmetric(DelimitedParser.byte)}
  short is symmetric                              ${symmetric(DelimitedParser.short)}
  int is symmetric                                ${symmetric(DelimitedParser.int)}
  long is symmetric                               ${symmetric(DelimitedParser.long)}
  double is symmetric                             ${symmetric(DelimitedParser.double)}
  boolean is symmetric                            ${symmetric(DelimitedParser.boolean)}
  string is symmetric                             ${symmetricWith(DelimitedParser.string)(identity)}
  success always succeeds                         ${alwaysSucceeds}
  fail always fails                               ${alwaysFails}


Convenience methods
===================

  ${ emptyString.run("", '|') ==== DelimitedParser.empty("").run("", '|') }
  ${ emptyList.run("", '|')   ==== DelimitedParser.empty(Nil).run("", '|') }

  ${ prop((list: List[Double]) =>
      doubleOrZero.run(list.map(_.toString).mkString("|"), '|') ==== (DelimitedParser.empty(0.0) ||| double).run(list.map(_.toString).mkString("|"), '|')) }

  ${ prop((list: List[Int]) =>
      intOrZero.run(list.map(_.toString).mkString("|"), '|') ==== (DelimitedParser.empty(0) ||| int).run(list.map(_.toString).mkString("|"), '|')) }

  ${ prop((list: List[String]) =>
      string.whenEmpty("A").run(list.mkString("|"), '|') ==== (DelimitedParser.empty("A") ||| string).run(list.mkString("|"), '|')) }

"""

  def position1 = {
    getPosition.run("", '|').toOption must beSome(0)
    (for {
      _  <- string
      p1 <- getPosition
      _  <- string
      p2 <- getPosition
    } yield (p1, p2)).run("a|b", '|').toOption must beSome((2, 3))
  }

  def string1 =
    string.run("a", '|').toOption must beSome("a")

  def stringOpt1 =
    stringOpt.run("a", '|').toOption must beSome(Some("a"))

  def stringOpt2 =
    stringOpt.run("", '|').toOption must beSome(None)

  def lengthstring1 = {
    string.oflength(4).run("abcd", '|').toOption must beSome("abcd")
    string.oflength(4, 6).run("abc", '|').toOption must beNone
    string.oflength(4, 6).run("abcdefg", '|').toOption must beNone
    string.oflength(4, 6).run("abcd", '|').toOption must beSome("abcd")
    string.oflength(4, 6).run("abcde", '|').toOption must beSome("abcde")
    string.oflength(4, 6).run("abcdef", '|').toOption must beSome("abcdef")
    string.oflength(4).run("ab", '|').toOption must beNone
    string.oflength(4).run("", '|').toOption must beNone
  }

  def optionlengthstring1 = {
    string.option.oflengthifsome(4).run("abcd", '|') must_== Some("abcd").success
    string.option.oflengthifsome(3, 5).run("abcd", '|') must_== Some("abcd").success
    string.option.oflengthifsome(3, 5).run("ab", '|').toOption must beNone
    string.option.oflengthifsome(4).run("ab", '|').toOption must beNone
    string.option.oflengthifsome(4).run("", '|') must_== None.success
  }

  def intlist1 = {
    (int +).run("1|2|3", '|') must_== List(1, 2, 3).success
    (int +).run("", '|').toOption must beNone
  }

  def intlist2 = {
    (int *).run("1|2|3", '|') must_== List(1, 2, 3).success
    (int *).run("", '|') must_== List[Int]().success
  }

  def nonemptystring1 = {
    string.nonempty.run("a", '|').toOption must beSome("a")
    string.nonempty.run("", '|').toOption must beNone
  }

  def emptyValue =
     DelimitedParser.empty(1).run("", '|').toEither must beRight(1)

  def int1 = {
    int.run("1", '|').toOption must beSome(1)
    int.run("1.0", '|').toOption must beNone

    // failure message with position
    (for {
      i1 <- int
      i2 <- int
    } yield (i1, i2)).parse("2|a", '|') must failAt(3, "not an int: 'a'")
  }

  def short1 = {
    short.run("1", '|').toOption must beSome(1.toShort)
    short.run("-1", '|').toOption must beSome(-1.toShort)
    short.run("1.0", '|').toOption must beNone
    short.run("32768", '|').toOption must beNone
    short.run("-32769", '|').toOption must beNone

    // failure message with position
    (for {
      s1 <- short
      s2 <- short
    } yield (s1, s2)).parse("2|a", '|') must failAt(3, "not a short: 'a'")
  }

  def byte1 = {
    byte.run("1", '|').toOption must beSome(1.toByte)
    byte.run("-1", '|').toOption must beSome(-1.toByte)
    byte.run("1.0", '|').toOption must beNone
    byte.run("128", '|').toOption must beNone
    byte.run("-129", '|').toOption must beNone

    // failure message with position
    (for {
      b1 <- byte
      b2 <- byte
    } yield (b1, b2)).parse("2|a", '|') must failAt(3, "not a byte: 'a'")
  }

  def char1 = prop((c: Char) =>
    DelimitedParser.char.run(c.toString, (c + 1).toChar).toOption must beSome(c))

  def invalidChar1 = prop((str: String) => (str.length != 1) ==> {
    DelimitedParser.char.run(str, '|').toOption must beNone })

  def double1 = {
    double.run("1.0", '|').toOption must beSome(1.0)
    double.run("1", '|').toOption must beSome(1.0)
    double.run("a", '|').toOption must beNone

    // failure message with position
    (for {
      d1 <- double
      d2 <- double
    } yield (d1, d2)).parse("2|a", '|') must failAt(3, "not a double: 'a'")
  }

  def localDate1 = {
    localDate.run("2013-03-11", '|').toOption must beSome(new LocalDate(2013, 3, 11))
    localDate.run("blah", '|').toOption must beNone

    // failure message with position
    (for {
      i1 <- int
      i2 <- localDate
    } yield (i1, i2)).parse("2|blah", '|') must failAt(6, startWith("not a local date with format yyyy-MM-dd: 'blah'"))
  }


  def consume1 =
    consume(3).run("a|b|c", '|').toOption must beSome(())

  def consume2 = {
    (for {
      i1 <- int
      i2 <- consumeRest
    } yield (i1, i2)).run("2|blah", '|').toOption must beSome((2, ()))

    consumeRest.run("a|b|c", '|').toOption must beSome(())
  }

  def value1 =
    DelimitedParser.valueOr(???, (t: Throwable) => "failed").run("", '|').toEither must beLeft("\nfailed (position: 0)")

  def validation1 =
    DelimitedParser.value("a".success).run("", '|').toOption must beSome("a")

  def multi1 = (for {
    s1 <- string
    i  <- int
    s2 <- string
  } yield (s1, i, s2)).run("a|1|b", '|').toOption must beSome(("a", 1, "b"))

  def leftover1 =
    string.run("a|b", '|').toEither must beLeft(
      """|Parsed successfully: a|b up to position 2
         | -> but the rest of the list was not consumed: b""".stripMargin)

  def preprocess1 =
    string.preprocess(_ => "b").run("a", '|').toOption must beSome("b")

  def fail1 = {
    val parser = for {
      s1 <- string
      i  <- int
      s2 <- string
    } yield (s1, i, s2)

    parser.run("aaaaaaaaaaaaaaa|bbb|ccccc", '|').toEither must beLeft(
      """|aaaaaaaaaaaaaaa|bbb|ccccc
         |not an int: 'bbb' (position: 20)""".stripMargin)
  }

  def named = prop { str: String =>
    val parser = fail("this is a failed parser").named("field number 1")
    parser.run(str, '|').toEither must beLeft(contain("field number 1"))
  }

  def orCombinator1 = prop((msg: String, str: Char) =>
    (fail(msg) ||| string).run(str.toString, '|') ==== str.toString.success)

  def orCombinator2 = prop((msg: String, str: Char) =>
    (string ||| fail(msg)).run(str.toString, '|') ==== str.toString.success)

  def orCombinator3 = prop((msg1: String, msg2: String, str: String) =>
    (fail(msg1) ||| fail(msg2)).run(str, '|').toEither must beLeft)

  def option1 =
    DelimitedParser.int.option.run("123", '|') must_== Some(123).success

  def option2 =
    DelimitedParser.int.option.run("", '|') must_== None.success

  def option3 =
    DelimitedParser.int.option.run("not-an-int", '|').toOption must_== None

  def alwaysSucceeds = prop((n: Int, s: String) =>
    DelimitedParser.success(n).parse(s, '|').toOption must beSome((DelimitedParserState(0, s, '|'), n)))

  def alwaysFails = prop((e: String, s: List[String]) =>
    DelimitedParser.fail(e).run(s.mkString("|"), '|').toOption must beNone)

  def symmetric[A: Arbitrary: Show](p: DelimitedParser[A]) =
    symmetricWith(p)(_.shows)

  def symmetricWith[A: Arbitrary](p: DelimitedParser[A])(toString: A => String) =
    prop((a: A) => p.run(toString(a), '|').toOption must beSome(a))

  /**
   * TEST METHODS
   */
  def failAt[A](position: Int, check: ValueCheck[String]): Matcher[ParseResult[A]] =  { result: ParseResult[A] =>
    result.toEither must beLeft { r: (DelimitedParserState, String) =>
      { r._1.position must_== position } and check.check(r._2)
    }
  }

  case class Delimiter(d: Char) { def s = d.toString }

  implicit def ArbitraryDelimiter: Arbitrary[Delimiter] = Arbitrary {
    Gen.oneOf(',', '|', '~').map(Delimiter)
  }

  /** Delimiter distinct from the delimiter above */
  case class Delimiter2(d: Char) { def s = d.toString }

  implicit def ArbitraryDelimiter2: Arbitrary[Delimiter2] = Arbitrary {
    Gen.oneOf(':', '=').map(Delimiter2)
  }

  case class SimpleString(s: String)
  implicit def ArbitrarySimpleString: Arbitrary[SimpleString] = Arbitrary {
    for {
      n  <- Gen.choose(1, 10)
      ss <- Gen.listOfN(n, Gen.oneOf('a', 'z'))
    } yield SimpleString(ss.mkString)
  }

  def simpleString: DelimitedParser[SimpleString] = string.map(SimpleString)

}
