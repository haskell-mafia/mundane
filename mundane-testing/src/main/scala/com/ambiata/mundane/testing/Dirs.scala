package com.ambiata.mundane.testing

import java.io.File
import java.util.UUID

// FIX this should be production code

trait Dirs {

  def mkRandomDir(prefix: String = "", base: File): File = {
    val d = mkdir(s"${prefix}${UUID.randomUUID()}", base)
    d.mkdirs()
    d
  }

  def mkdir(name: String, base: File): File = {
    val d = new File(base, name)
    d.mkdirs()
    d
  }

  def mkTempDir(prefix: String, suffix: String = System.nanoTime.toString): File = {
    val tmpFile = File.createTempFile(prefix, suffix)
    if (!tmpFile.delete) sys.error(s"Could not delete temp file '${tmpFile.getAbsolutePath}'")
    if (!tmpFile.mkdir)  sys.error(s"Could not create temp dir '${tmpFile.getAbsolutePath}'")
    tmpFile
  }

  def rmdir(d: File) {
    if(d.isDirectory) d.listFiles.foreach(rmdir) else d.delete
    d.delete
  }
}

object Dirs extends Dirs
