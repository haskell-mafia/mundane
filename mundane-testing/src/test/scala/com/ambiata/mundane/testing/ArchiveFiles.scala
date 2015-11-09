package com.ambiata.mundane.testing

import java.io.{File, FileOutputStream, PrintStream}

// FIX this should be production code
trait ArchiveFiles {
  def tarball(inDir: File, outFile: File): File = {
    import scala.sys.process._
    Process(List("sh", "-c", s"tar czf ${outFile.getPath} --directory ${inDir.getPath} .")).run(ProcessLogger(stdout => (), println))
    outFile
  }

  def gzip(file: File): File = {
    import scala.sys.process._
    val gzfile = new File(file.getPath + ".gz")
    Process(List("sh", "-c", s"gzip -c ${file.getPath} > ${gzfile.getPath}")).run(ProcessLogger(o => (), println))
    gzfile
  }

  def writeBytes(f: File, bytes: Array[Byte]) {
    val fos =  new FileOutputStream(f)
    fos.write(bytes)
    fos.close()
  }

  def writeString(f: File, str: String, encoding: String = "UTF-8") {
    val fos = new FileOutputStream(f)
    val ps = new PrintStream(fos, false, encoding)
    ps.print(str)
    ps.close()
    fos.close()
  }

}
