package com.ambiata.mundane.csv

import CsvArbitraries._
import org.specs2._
import org.specs2.matcher.DisjunctionMatchers
import org.specs2.specification.Fragments

class SimpleCsvParserSpec extends Specification with ScalaCheck with DisjunctionMatchers { def is = s2"""

 A csv parser can deal with
   comma-separated values $csv
   quoted fields          $quoted

 but it does not reject some lines with unquoted characters in quoted fields $malformed

 Common test case sets
   Wikipedia tests (http://en.wikipedia.org/wiki/Module_talk:CSV/testcases)       $wikipedia
   csv-spectrum tests (https://github.com/maxogden/csv-spectrum)                  $csvSpectrum
                                                                                   """

  def csv = prop { csvLine: CsvLine =>
    SimpleCsv.csv.parse(csvLine.value) must be_\/-(csvLine.expected)
  }

  def quoted = prop { csvLine: CsvQuotedLine =>
    SimpleCsv.csv.parse(csvLine.value) must be_\/-(csvLine.expected)
  }

  def malformed = {
    SimpleCsv.csv.parse("\"w\"t\",\"v\"y\"").aka must be_\/-(List("w\"t\",\"v\"y"))
  }

  def wikipedia = {
    CsvSampleData.wikipedia.foldLeft(Fragments()) {
      case (res, CsvData(name, rows, expectedResults, delimiter, quoteChar)) =>
        res ^
         rows.zip(expectedResults).foldLeft(Fragments()) { case (result, (row, expected)) =>
           result ^ name ! {
             SimpleCsv(delimiter.getOrElse(','), quoteChar, None).parse(row) must be_\/-(expected)
           } ^ br
         } ^ br
    }
  }

  def csvSpectrum = {
    CsvSampleData.csvSpectrum.foldLeft(Fragments.create(br)) {
      case (res, CsvData(name, rows, expectedResults, delimiter, quoteChar)) =>
        res ^
          rows.zip(expectedResults).foldLeft(Fragments.create(br)) { case (result, (row, expected)) =>
            result ^ name ! {
              SimpleCsv(delimiter.getOrElse(','), quoteChar, None).parse(row) must be_\/-(expected)
            } ^ br
          } ^ br
    }
  }

}

