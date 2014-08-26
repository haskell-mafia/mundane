package com.ambiata.mundane.parse.benchmark

import com.google.caliper._

object ParserBenchApp extends App {
  Runner.main(classOf[ParserBench], args)
}

class ParserBench extends SimpleScalaBenchmark {

  def time_ermine_json(reps: Int): Unit =
    repeat(reps)(Ermine.JsonParser.parsing(Json.text))

  def time_ermine_json2(reps: Int): Unit =
    repeat(reps)(Ermine.JsonParser.parsing(Json.text2))

  def time_atto_json(reps: Int): Unit =
    repeat(reps)(Atto.parse(Atto.JsonExample.jexpr, Json.text))

//  def time_atto_json2(reps: Int): Unit =
//    repeat(reps)(Atto.parse(Atto.JsonExample.jexpr, Json.text2))

  def time_parboided_json(reps: Int): Unit =
    repeat(reps)(new Parboil.JsonParser(Json.text).Json.run().get)

  def time_parboided_json2(reps: Int): Unit =
    repeat(reps)(new Parboil.JsonParser(Json.text2).Json.run().get)
}
