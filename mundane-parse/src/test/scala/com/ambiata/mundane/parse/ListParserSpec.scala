package com.ambiata.mundane
package parse

import scalaz._, Scalaz._
import org.specs2._
import org.specs2.matcher.ThrownExpectations
import ListParser._
import org.joda.time._

class ListParserSpec extends Specification with ThrownExpectations { def is = s2"""

 Parsing a list can:
   extract the position of each element $position1
   extract a string                     $string1
   extract a nonempty string            $nonemptystring1
   extract an int                       $int1
   extract a double                     $double1
   extract a LocalDate                  $localDate1
   consume elements                     $consume1
   consume all remaining elements       $consume2
   lift a value                         $value1
   lift a validation                    $validation1
   extract multiple values              $multi1
   error if not parsed all elements     $leftover1
   preprocess the input string          $preprocess1
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
    } yield (i1, i2)).run(List("2", "a")).toEither must beLeft("""Not an int at position 2: 'a'""")
  }

  def double1 = {
    double.run(List("1.0")).toOption must beSome(1.0)
    double.run(List("1")).toOption must beSome(1.0)
    double.run(List("a")).toOption must beNone

    // failure message with position
    (for {
      d1 <- double
      d2 <- double
    } yield (d1, d2)).run(List("2", "a")).toEither must beLeft("""Not a double at position 2: 'a'""")
  }

  def localDate1 = {
    localDate.run(List("2013-03-11")).toOption must beSome(new LocalDate(2013, 3, 11))
    localDate.run(List("blah")).toOption must beNone

    // failure message with position
    (for {
      i1 <- int
      i2 <- localDate
    } yield (i1, i2)).run(List("2", "blah")).toEither must beLeft(startWith("""Not a local date with format yyyy-MM-dd at position 2: 'blah'"""))
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
    ListParser.value(???, (t: Throwable) => "failed").run(Nil).toEither must beLeft("failed")

  def validation1 =
    ListParser.value("a".success).run(Nil).toOption must beSome("a")

  def multi1 = (for {
    s1 <- string
    i  <- int
    s2 <- string
  } yield (s1, i, s2)).run(List("a", "1", "b")).toOption must beSome(("a", 1, "b"))

  def leftover1 =
    string.run(List("a", "b")).toEither must beLeft("There was more input not consumed: List(b)")

  def preprocess1 =
    string.preprocess(_ => "b").run(List("a")).toOption must beSome("b")

}