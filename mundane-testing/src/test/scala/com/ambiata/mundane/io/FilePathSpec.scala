package com.ambiata.mundane.io

import org.specs2.Specification

class FilePathSpec extends Specification { def is = s2"""

 It is possible to get the parent of a FilePath
   ${ FilePath("test/hello/world").parent must beSome(FilePath("test/hello")) }

 It is possible to get the root of a FilePath
   ${ FilePath("test/hello/world").rootname === FilePath("test") }

 It is possible to get the portion of a FilePath from the root
   ${ FilePath("test/hello/world").fromRoot === FilePath("hello/world") }

 It is possible to get a the portion of a FilePath that is relative to another one
   ${ FilePath("test/hello/world/eric").relativeTo(FilePath("test/hello"))  === FilePath("world/eric") }
   ${ FilePath("test/hello/world/eric").relativeTo(FilePath("other/hello")) === FilePath("test/hello/world/eric") }

"""
}
