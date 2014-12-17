package com.ambiata.mundane.io

import com.ambiata.mundane.io.Arbitraries._
import com.ambiata.mundane.testing.RIOMatcher._
import com.ambiata.mundane.path._

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

  def read = prop((s: String, local: LocalTemporary) => for {
    p <- local.file
    _ <- Files.write(p, s)
    d <- Files.read(p)
  } yield d ==== s)

  def readBytes = prop((bs: Array[Byte], local: LocalTemporary) => for {
    p <- local.file
    _ <- Files.writeBytes(p, bs)
    d <- Files.readBytes(p)
  } yield d ==== bs)

  def unicode = prop((local: LocalTemporary) => {
    val data = """섋騚㊼
乡왇㛩鴄〫⑁䨜嵏风녇佞ው煓괄ꎮꒀ醆魓ﰺ評떜뻀썲荘㳰锉䤲߶㊢ᅫ㠏⴫⃅⒊逢墵⍓刹军"""
    for {
      p <- local.file
      _ <- Files.write(p, data)
      d <- Files.read(p)
    } yield d ==== data
  })

  def readLines = prop((lines: List[String], local: LocalTemporary) => {
    val linesWithNoNewline = lines.map(_.replaceAll("\\s", ""))
    for {
      p <- local.file
      _ <- Files.writeLines(p, linesWithNoNewline)
      d <- Files.readLines(p)
    } yield d ==== linesWithNoNewline.toVector
  }).set(minTestsOk = 1000)

  def readTrailing = prop((string: String, local: LocalTemporary) => {
    // see issue #67
    // prepareForFile(linesOf(filecontent("abcd\n"))) == filecontent("abcd\n")
    val stringWithTrailingNewline = string.replaceAll("\\s", "") +"\n"
    val lines = stringWithTrailingNewline.lines.toSeq
    for {
      p <- local.file
      _ <- Files.writeLines(p, lines)
      l <- Files.read(p)
    } yield l ==== stringWithTrailingNewline
  }).set(minTestsOk = 1000)
}
