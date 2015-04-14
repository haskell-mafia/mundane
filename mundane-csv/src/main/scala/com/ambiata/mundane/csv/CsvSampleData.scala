package com.ambiata.mundane.csv

/**
 * Sample data to test csv parsers
 */
object CsvSampleData {
  
  def wikipedia =
    List(
      CsvData(
        "Basic numbers",
        List("1,2,3", "4,5,6", "7,8,9"),
        List(List("1","2","3"), List("4","5","6"), List("7","8","9")))
      , CsvData(
        "Basic strings",
        List("Lorem,ipsum,dolor,sit", "amet,consectetur", "adipisicing,elit,sed,do,eiusmod"),
        List(List("Lorem","ipsum","dolor","sit"), List("amet","consectetur"), List("adipisicing","elit","sed","do","eiusmod")))
      , CsvData(
        "Quoted strings",
        List("Lorem,\"ipsum, dolor, sit\",amet", "Consectetur,adipisicing,\"elit, sed,\",eiusmod"),
        List(List("Lorem","ipsum, dolor, sit","amet"), List("Consectetur","adipisicing","elit, sed,","eiusmod")))
      , CsvData(
        "Empty field",
        List("1,2,,3"),
        List(List("1","2","","3")))
      , CsvData(
        "Trailing empty field",
        List("1,2,3,"),
        List(List("1","2","3","")))
      , CsvData(
        "Leading empty field",
        List(",1,2,3"),
        List(List("","1","2","3")))
      , CsvData(
        "Escaped quote",
        List("1,\"2\"\"3\",4"),
        List(List("1","2\"3","4")))
      , CsvData(
        "Field starts with escaped double quote",
        List(s""""$escapedQuote--""""),
        List(List("\"--")))
      , CsvData(
        "Field end with escaped double quote",
        List(s""""--$escapedQuote""""),
        List(List("--\"")))
      , CsvData(
        "One, two or three escaped double quotes in strings",
        List(s""""$escapedQuote"""",s""""$escapedQuote$escapedQuote"""",s""""$escapedQuote$escapedQuote$escapedQuote""""),
        List(List("\""), List("\"\""), List("\"\"\"")))
      , CsvData(
        "Specified delimiter",
        List("1;2;3", "4;5;6"),
        List(List("1","2","3"), List("4","5","6")), delimiter = Some(';'))
      , CsvData(
        "Specified quote char",
        List("1,`2,3`,4"),
        List(List("1","2,3","4")), quoteChar = Some('`'))
      , CsvData(
        "Spaces in fields",
        List(", 1 ,\" 2 \","),
        List(List(""," 1 ", " 2 ","")))
      , CsvData(
        "Single empty field",
        List(""),
        List(List("")))
      , CsvData(
        "Two blank fields",
        List(","),
        List(List("", "")))
      , CsvData(
        "Field begins with comma",
        List("\",a\""),
        List(List(",a")))
    )

  def wikipediaLines(replication: Int): List[String] =
    wikipedia.flatMap(test => List.fill(replication)(test.rows).flatten)

  val escapedQuote = "\"\""

  def csvSpectrum =
    List(
      CsvData(
        "comma in quotes",
        List("John,Doe,120 any st.,\"Anytown, WW\",08123"),
        List(List("John","Doe","120 any st.","Anytown, WW","08123"))
      )
      , CsvData(
        "empty",
        List("1,\"\",\"\"","2,3,4"),
        List(List("1","",""), List("2","3","4"))
      )
      , CsvData(
        "escaped quotes",
        List("1,\"ha \"\"ha\"\" ha\"", "3,4"),
        List(List("1","ha \"ha\" ha"), List("3","4"))
      )
      , CsvData(
        "json",
        List("1,\"{\"\"type\"\": \"\"Point\"\", \"\"coordinates\"\": [102.0, 0.5]}\""),
        List(List("1","{\"type\": \"Point\", \"coordinates\": [102.0, 0.5]}"))
      )
      , CsvData(
        "newlines",
        List("1,2,3","\"Once upon\na time\",5,6", "7,8,9"),
        List(List("1","2","3"), List("Once upon\na time","5","6"), List("7","8","9"))
      )
      , CsvData(
        "newlines crlf",
        List("1,2,3","\"Once upon\r\na time\",5,6", "7,8,9"),
        List(List("1","2","3"), List("Once upon\r\na time","5","6"), List("7","8","9"))
      )
      , CsvData(
        "quotes and newlines",
        List("1,\"ha\n\"\"ha\"\"\nha\"", "3,4"),
        List(List("1","ha\n\"ha\"\nha"), List("3","4"))
      )
    )

}

case class CsvData(name: String, rows: List[String], expected: List[List[String]],
                   delimiter: Option[Char] = None,
                   quoteChar: Option[Char] = None)



