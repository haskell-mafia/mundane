package com.ambiata.mundane
package csv

import com.ambiata.disorder._
import org.scalacheck._, Arbitrary._, Gen._

object CsvArbitraries {
  case class CsvLine(value: String, expected: List[String])

  implicit def ArbitraryCsvLine: Arbitrary[CsvLine] =
    Arbitrary(Gen.sized(n => listOfN(n, genRegularField).map(fs => CsvLine(fs.mkString(","), if (fs.isEmpty) List("") else fs))))

  case class CsvQuotedLine(value: String, expected: List[String])

  implicit def ArbitraryCsvQuotedLine: Arbitrary[CsvQuotedLine] =
    Arbitrary(Gen.sized(n => listOfN(n, genQuotedField)
      .map(fs =>
      CsvQuotedLine(
        if (fs.isEmpty) ""
        else fs.mkString("\"", "\",\"", "\""),
        if (fs.isEmpty) List("") else fs.map(_.replace("\"\"", "\""))))))

  // expected double-quotes to be removed
  case class CsvMalformedLine(value: String)

  implicit def ArbitraryCsvMalformedLine: Arbitrary[CsvMalformedLine] =
    Arbitrary(Gen.sized(n => listOfN(n + 1, genMalformedField) // generate at least one malformed field
      .map(fs =>
      CsvMalformedLine(
        if (fs.isEmpty) ""
        else fs.mkString("\"", "\",\"", "\""))))) // expected double-quotes to be removed


  def genRegularField: Gen[String] =
    arbitrary[Ident].map(_.value)

  def genQuotedField: Gen[String] =
    for {
      q <- Gen.oneOf("\"\"", ",", "") // q might be a double quote a delimiter or empty
      a <- genRegularField
      b <- genRegularField
    } yield s"$a$q$b"

  def genMalformedField: Gen[String] =
    for {
      q <- Gen.oneOf("\"") // a non-escaped quote will fail the parse
      a <- genRegularField
      b <- genRegularField
    } yield s"$a$q$b"

}



