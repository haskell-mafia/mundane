package com.ambiata.mundane
package parse

import scalaz._, Scalaz._
import org.specs2._
import org.specs2.matcher.{ValueCheck, Matcher, ThrownExpectations}
import org.scalacheck._
import ListParser._
import org.joda.time._

class ListParserSpec extends Specification with ThrownExpectations with ScalaCheck { def is = s2"""

Examples
========

 Parsing a list will:
   extract the position of each element                                              $position1
   extract a string                                                                  $string1
   extract a string option                                                           $stringOpt1
   extract a string option from an empty list                                        $stringOpt2
   extract a nonempty string                                                         $nonemptystring1
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

Properties
==========

  byte is symmetric                               ${symmetric(ListParser.byte)}
  short is symmetric                              ${symmetric(ListParser.short)}
  int is symmetric                                ${symmetric(ListParser.int)}
  long is symmetric                               ${symmetric(ListParser.long)}
  double is symmetric                             ${symmetric(ListParser.double)}
  boolean is symmetric                            ${symmetric(ListParser.boolean)}
  string is symmetric                             ${symmetricWith(ListParser.string)(identity)}
  success always succeeds                         ${alwaysSucceeds}
  fail always fails                               ${alwaysFails}


"""

  def position1 = {
    getPosition.run(List()).toOption must beSome(0)
    (for {
      _  <- string
      p1 <- getPosition
      _  <- string
      p2 <- getPosition
    } yield (p1, p2)).run(List("a", "b")).toOption must beSome((1, 2))
  }

  def string1 =
    string.run(List("a")).toOption must beSome("a")

  def stringOpt1 =
    stringOpt.run(List("a")).toOption must beSome(Some("a"))

  def stringOpt2 =
    stringOpt.run(Nil).toOption must beSome(None)

  def lengthstring1 = {
    string.oflength(4).run(List("abcd")).toOption must beSome("abcd")
    string.oflength(4, 6).run(List("abc")).toOption must beNone
    string.oflength(4, 6).run(List("abcdefg")).toOption must beNone
    string.oflength(4, 6).run(List("abcd")).toOption must beSome("abcd")
    string.oflength(4, 6).run(List("abcde")).toOption must beSome("abcde")
    string.oflength(4, 6).run(List("abcdef")).toOption must beSome("abcdef")
    string.oflength(4).run(List("ab")).toOption must beNone
    string.oflength(4).run(List("")).toOption must beNone
  }

  def optionlengthstring1 = {
    string.option.oflengthifsome(4).run(List("abcd")) must_== Some("abcd").success
    string.option.oflengthifsome(3, 5).run(List("abcd")) must_== Some("abcd").success
    string.option.oflengthifsome(3, 5).run(List("ab")).toOption must beNone
    string.option.oflengthifsome(4).run(List("ab")).toOption must beNone
    string.option.oflengthifsome(4).run(List("")) must_== None.success
  }

  def intlist1 = {
    (int +).run(List("1", "2", "3")) must_== List(1, 2, 3).success
    (int +).run(List()).toOption must beNone
  }

  def intlist2 = {
    (int *).run(List("1", "2", "3")) must_== List(1, 2, 3).success
    (int *).run(List()) must_== List[Int]().success
  }

  def nonemptystring1 = {
    string.nonempty.run(List("a")).toOption must beSome("a")
    string.nonempty.run(List("")).toOption must beNone
  }

  def int1 = {
    int.run(List("1")).toOption must beSome(1)
    int.run(List("1.0")).toOption must beNone

    // failure message with position
    (for {
      i1 <- int
      i2 <- int
    } yield (i1, i2)).parse(List("2", "a")) must failAt(2, "not an int: 'a'")
  }

  def short1 = {
    short.run(List("1")).toOption must beSome(1.toShort)
    short.run(List("-1")).toOption must beSome(-1.toShort)
    short.run(List("1.0")).toOption must beNone
    short.run(List("32768")).toOption must beNone
    short.run(List("-32769")).toOption must beNone

    // failure message with position
    (for {
      s1 <- short
      s2 <- short
    } yield (s1, s2)).parse(List("2", "a")) must failAt(2, "not a short: 'a'")
  }

  def byte1 = {
    byte.run(List("1")).toOption must beSome(1.toByte)
    byte.run(List("-1")).toOption must beSome(-1.toByte)
    byte.run(List("1.0")).toOption must beNone
    byte.run(List("128")).toOption must beNone
    byte.run(List("-129")).toOption must beNone

    // failure message with position
    (for {
      b1 <- byte
      b2 <- byte
    } yield (b1, b2)).parse(List("2", "a")) must failAt(2, "not a byte: 'a'")
  }

  def char1 = prop((c: Char) =>
    ListParser.char.run(List(c.toString)).toOption must beSome(c))

  def invalidChar1 = prop((str: String) => (str.length != 1) ==> {
    ListParser.char.run(List(str)).toOption must beNone })

  def double1 = {
    double.run(List("1.0")).toOption must beSome(1.0)
    double.run(List("1")).toOption must beSome(1.0)
    double.run(List("a")).toOption must beNone

    // failure message with position
    (for {
      d1 <- double
      d2 <- double
    } yield (d1, d2)).parse(List("2", "a")) must failAt(2, "not a double: 'a'")
  }

  def localDate1 = {
    localDate.run(List("2013-03-11")).toOption must beSome(new LocalDate(2013, 3, 11))
    localDate.run(List("blah")).toOption must beNone

    // failure message with position
    (for {
      i1 <- int
      i2 <- localDate
    } yield (i1, i2)).parse(List("2", "blah")) must failAt(2, startWith("not a local date with format yyyy-MM-dd: 'blah'"))
  }


  def consume1 =
    consume(3).run(List("a", "b", "c")).toOption must beSome(())

  def consume2 = {
    (for {
      i1 <- int
      i2 <- consumeRest
    } yield (i1, i2)).run(List("2", "blah")).toOption must beSome((2, ()))

    consumeRest.run(List("a", "b", "c")).toOption must beSome(())
  }

  def value1 =
    ListParser.valueOr(???, (t: Throwable) => "failed").run(Nil).toEither must beLeft("failed")

  def validation1 =
    ListParser.value("a".success).run(Nil).toOption must beSome("a")

  def multi1 = (for {
    s1 <- string
    i  <- int
    s2 <- string
  } yield (s1, i, s2)).run(List("a", "1", "b")).toOption must beSome(("a", 1, "b"))

  def leftover1 =
    string.run(List("a", "b")).toEither must beLeft(
      """|Parsed successfully: List(a, b) up to position 1
         | -> but the rest of the list was not consumed: List(b)""".stripMargin)

  def preprocess1 =
    string.preprocess(_ => "b").run(List("a")).toOption must beSome("b")

  def fail1 = {
    val parser = for {
      s1 <- string
      i  <- int
      s2 <- string
    } yield (s1, i, s2)

    parser.run(List("aaaaaaaaaaaaaaa", "bbb", "ccccc")).toEither must beLeft(
      """|aaaaaaaaaaaaaaa, bbb, ccccc
         |not an int: 'bbb' (position: 2)""".stripMargin)
  }

  def orCombinator1 = prop((msg: String, str: String) =>
    ((fail(msg) ||| string).run(List(str)) ==== str.success))

  def orCombinator2 = prop((msg: String, str: String) =>
    ((string ||| fail(msg)).run(List(str)) ==== str.success))

  def orCombinator3 = prop((msg1: String, msg2: String, str: String) =>
    ((fail(msg1) ||| fail(msg2)).run(List(str)).toEither must beLeft))

  def option1 =
    ListParser.int.option.run(List("123")) must_== Some(123).success

  def option2 =
    ListParser.int.option.run(List("")) must_== None.success

  def option3 =
    ListParser.int.option.run(List("not-an-int")).toOption must_== None

  def alwaysSucceeds = prop((n: Int, s: List[String]) =>
    ListParser.success(n).parse(s).toOption must beSome((0, s, n)))

  def alwaysFails = prop((e: String, s: List[String]) =>
    ListParser.fail(e).run(s).toOption must beNone)

  def symmetric[A: Arbitrary: Show](p: ListParser[A]) =
    symmetricWith(p)(_.shows)

  def symmetricWith[A: Arbitrary](p: ListParser[A])(toString: A => String) =
    prop((a: A) => p.run(List(toString(a))).toOption must beSome(a))

  /**
   * TEST METHODS
   */
  def failAt[A](position: Int, check: ValueCheck[String]): Matcher[ParseResult[A]] =  { result: ParseResult[A] =>
    result.toEither must beLeft { r: (Int, String) =>
      { r._1 must_== position } and check.check(r._2)
    }
  }


}
