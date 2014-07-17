package com.ambiata.mundane.io

import org.specs2.Specification

class MemoryConversionsSpec extends Specification with MemoryConversions { def is = s2"""
  ${ 1024.bytes.toKilobytes === 1.kb }
  ${ 1024.kb.toMegabytes    === 1.mb }
  ${ 1024.mb.toGigabytes    === 1.gb }
  ${ 1024.gb.toTerabytes    === 1.tb }

  ${ 1.kb.toBytes     === 1024.bytes }
  ${ 1.mb.toKilobytes === 1024.kbs }
  ${ 1.gb.toMegabytes === 1024.mbs }
  ${ 1.tb.toGigabytes === 1024.gbs }


  We can sum byte quantities in a list
  ${ List(1.kb, 2.mb, 3.bytes).sum === (1024 + 2*1024*1024 + 3).bytes }
"""
}
