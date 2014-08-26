package com.ambiata.mundane.parse.benchmark

object Ermine {

  import scalaparsers._
  import Json._

  object JsonParser extends scalaparsers.Parsing[Unit] {

    def parsing[A](content: String): JValue = {
      parser.run(ParseState(Pos.start("", content), content, s = ()), Supply.create) match {
        case Right((s, a)) => a
        case Left(e) => sys.error("ERROR " + e.toString())
      }
    }

    lazy val parser: Parser[JValue] = for {
      _ <- simpleSpace.skipMany
      x <- choice(
        stringLiteral.map(JString.apply),
        obj,
        array,
        boolean,
        double,
        nullString
      )
      _ <- simpleSpace.skipMany
    } yield x

    lazy val boolean: Parser[JValue] =
      choice(word("true").as(JBoolean(true)), word("false").as(JBoolean(false)))

    lazy val double: Parser[JValue] =
      (for {
        m <- ch('-').optional
        d <- nat
        t <- (ch('.') >> nat).optional
      } yield m.getOrElse("") + d.toString + t.getOrElse("")).map(l => JNumber(l.toDouble))

    lazy val nullString: Parser[JValue] =
      word("null").as(JNull)

    lazy val obj: Parser[JValue] =
      (for {
        _ <- simpleSpace.skipMany
        s <- stringLiteral
        _ <- ch(':')
        x <- parser
        _ <- simpleSpace.skipMany
      } yield (s, x)).sepBy(ch(',')).between("{", "}").map(JObject)

    lazy val array: Parser[JValue] =
      parser.sepBy(ch(',')).between("[", "]").map(JArray)
  }
}
