package com.ambiata.mundane.io

import com.ambiata.mundane.testing._
import com.ambiata.mundane.testing.ResultTIOMatcher._

import java.io._
import java.util.UUID

import org.specs2._, matcher._, specification._

import scalaz._, Scalaz._

class FilesSpec extends Specification with ScalaCheck with AfterExample { def is = isolated ^ s2"""

 Files should be able to:
  read string from file                       $read
  read bytes from file                        $readBytes
  read utf8 string with new line from file    $unicode
"""

  val work = System.getProperty("java.io.tmpdir", "/tmp") </> s"FilesSpec.${UUID.randomUUID}"

  def read = prop((s: String) => {
    val path = work </> "files-spec.string"
    val action = Files.write(path, s) >> Files.read(path)
    action must beOkValue(s)
  }).set(minTestsOk = 1000)

  def readBytes = prop((bs: Array[Byte]) => {
    val path = work </> "files-spec.bytes"
    val action = Files.writeBytes(path, bs) >> Files.readBytes(path)
    action must beOkValue(bs)
  })

  def unicode = {
    val data = """섋騚㊼
乡왇㛩鴄〫⑁䨜嵏风녇佞ው煓괄ꎮꒀ醆魓ﰺ評떜뻀썲荘㳰锉䤲߶㊢ᅫ㠏⴫⃅⒊逢墵⍓刹军"""
    val path = work </> "unicode"
    val action = Files.write(path, data) >> Files.read(path)
    action must beOkValue(data)
  }

  def after =
    Directories.delete(work).run.unsafePerformIO
}
