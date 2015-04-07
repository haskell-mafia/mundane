package com.ambiata.mundane.csv

import com.google.caliper._

object CsvParsersScalaBenchApp extends App {
  Runner.main(classOf[CsvParsersScalaBenchmark], args)
}

class CsvParsersScalaBenchmark extends SimpleScalaBenchmark {

  val lines = CsvSampleData.wikipediaLines(10)

  def timeBasicCsvParser(reps: Int): Unit =  {
    val parser = BasicCsv.csv
    for (i <- 0 to reps)
      lines.foreach(parser.parse)
  }

  def timeParboiledCsvParser(reps: Int): Unit =  {
    val parser = ParboiledCsv.csv
    for (i <- 0 to reps)
      lines.foreach(parser.parse)
  }

  def timeSimpleCsvParser(reps: Int): Unit =  {
    val parser = SimpleCsv.csv
    for (i <- 0 to reps)
      lines.foreach(parser.parse)
  }
}

trait SimpleScalaBenchmark extends SimpleBenchmark {

  // helper method to keep the actual benchmarking methods a bit cleaner
  // your code snippet should always return a value that cannot be "optimized away"
  def repeat[@specialized A](reps: Int)(snippet: => A) = {
    val zero = 0.asInstanceOf[A] // looks weird but does what it should: init w/ default value in a fully generic way
    var i = 0
    var result = zero
    while (i < reps) {
      val res = snippet
      if (res != zero) result = res // make result depend on the benchmarking snippet result
      i = i + 1
    }
    result
  }

}