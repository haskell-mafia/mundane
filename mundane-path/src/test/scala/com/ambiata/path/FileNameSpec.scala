package com.ambiata.mundane.path

import org.specs2.mutable.Specification

class FileNameSpec extends Specification {
  "A file name can be created with a literal" >> {
    val fs: FileName = "name"
    fs.name === "name"
  }

// this doesn't compile
//  "A file name can not be created with a literal containing a /" >> {
//    val fs: FileName = "na/me"
//    fs.name === "na/me"
//  }
}
