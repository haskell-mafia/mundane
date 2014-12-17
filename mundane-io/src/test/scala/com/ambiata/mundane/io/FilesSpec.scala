package com.ambiata.mundane.io

import com.ambiata.mundane.testing.RIOMatcher._
import org.specs2._
import scalaz._, Scalaz._

class FilesSpec extends Specification with ScalaCheck { def is = s2"""

 Files should be able to:
  read string from file                                    $read
  read bytes from file                                     $readBytes
  read utf8 string with new line from file                 $unicode
  read lines from file                                     $readLines
  read a lines with a trailing newline                     $readTrailing

"""

  def read = prop((s: String) => TemporaryDirPath.withDirPath { work =>
    val path = work <|> "files-spec.string"
    Files.write(path, s) >> Files.read(path)
  } must beOkValue(s)).set(minTestsOk = 1000)

  def readBytes = prop((bs: Array[Byte]) => TemporaryDirPath.withDirPath { work =>
    val path = work <|> "files-spec.bytes"
    Files.writeBytes(path, bs) >> Files.readBytes(path)
  } must beOkValue(bs))

  def unicode = {
    val data = """섋騚㊼
乡왇㛩鴄〫⑁䨜嵏风녇佞ው煓괄ꎮꒀ醆魓ﰺ評떜뻀썲荘㳰锉䤲߶㊢ᅫ㠏⴫⃅⒊逢墵⍓刹军"""
    TemporaryDirPath.withDirPath { work =>
      val path = work <|> "unicode"
      Files.write(path, data) >> Files.read(path)
    } must beOkValue(data)
  }

  def readLines = prop { lines: List[String] =>
    val linesWithNoNewline = lines.map(_.replaceAll("\\s", ""))

    TemporaryDirPath.withDirPath { work =>
      val path = work <|> "files-spec.string"
      Files.writeLines(path, linesWithNoNewline) >> Files.readLines(path)
    } must beOkValue(linesWithNoNewline.toVector)

  }.set(minTestsOk = 1000)

  def readTrailing = prop { string: String =>
    // see issue #67
    // prepareForFile(linesOf(filecontent("abcd\n"))) == filecontent("abcd\n")
    val stringWithTrailingNewline = string.replaceAll("\\s", "") +"\n"
    val lines = stringWithTrailingNewline.lines.toSeq
    TemporaryDirPath.withDirPath { work =>
      val path = work <|> "files-spec.string"
      Files.writeLines(path, lines) >> Files.read(path)
    } must beOkValue(stringWithTrailingNewline)

  }.set(minTestsOk = 1000)

}
