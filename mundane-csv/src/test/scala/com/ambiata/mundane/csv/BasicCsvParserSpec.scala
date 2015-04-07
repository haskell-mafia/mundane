package com.ambiata.mundane.csv

import CsvArbitraries._
import org.specs2._
import org.specs2.matcher.DisjunctionMatchers
import org.specs2.specification.Fragments

class BasicCsvParserSpec extends Specification with ScalaCheck with DisjunctionMatchers { def is = s2"""

 A csv parser can deal with
   comma-separated values $csv
   quoted fields          $quoted
   malformed lines        $malformed

 Common test case sets
   Wikipedia tests (http://en.wikipedia.org/wiki/Module_talk:CSV/testcases)       $wikipedia
   csv-spectrum tests (https://github.com/maxogden/csv-spectrum)                  $csvSpectrum
                                                                                   """

  def csv = prop { csvLine: CsvLine =>
    BasicCsv.csv.parse(csvLine.value) must be_\/-(csvLine.expected)
  }

  def quoted = prop { csvLine: CsvQuotedLine =>
    BasicCsv.csv.parse(csvLine.value) must be_\/-(csvLine.expected)
  }

  def malformed = prop { csvLine: CsvMalformedLine =>
    BasicCsv.csv.parse(csvLine.value) must be_-\/
  }

  def wikipedia = {
    CsvSampleData.wikipedia.foldLeft(Fragments.create(br)) {
      case (res, CsvData(name, rows, expectedResults, delimiter, quoteChar)) =>
        res ^
         rows.zip(expectedResults).foldLeft(Fragments.create(br)) { case (result, (row, expected)) =>
           result ^ name ! {
             BasicCsv(delimiter.getOrElse(','), quoteChar, Some('"')).parse(row) must be_\/-(expected)
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
              BasicCsv(delimiter.getOrElse(','), quoteChar, Some('"')).parse(row) must be_\/-(expected)
            } ^ br
          } ^ br
    }
  }
}

