package com.ambiata.mundane.testing

import java.io.{File, PrintStream}

// FIX this is messy and should not exists

trait LocalFiles {

  val basePath: File

  def createFile(name: String, content: String, base: File = basePath, encoding: String = "UTF-8"): File = {
    val t = new File(base, name)
    t.getParentFile.mkdirs
    val ps = new PrintStream(t, encoding)
    ps.print(content)
    ps.close()
    t
  }
}
