package com.ambiata.mundane.path

import org.specs2.mutable.Specification

class ComponentSpec extends Specification {
  "A file name can be created with a literal" >> {
    val fs: Component = Component("name")
    fs.name === "name"
  }

// this doesn't compile
//  "A file name can not be created with a literal containing a /" >> {
//    val fs: Component = "na/me"
//    fs.name === "na/me"
//  }
}
