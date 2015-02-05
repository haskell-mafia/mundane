package com.ambiata.mundane.io

import com.ambiata.mundane.control._
import com.ambiata.mundane.io.Temporary._
import java.util.concurrent.atomic.AtomicInteger
import scalaz._, Scalaz._

case class LocalTemporary(base: LocalPath, seed: String) {
  private val step: AtomicInteger = new AtomicInteger(0)

  def path: RIO[LocalPath] = {
    val path = setup
    val filePath = path /- java.util.UUID.randomUUID().toString
    run(s"LocalPath($filePath.path)") >>
      RIO.ok(filePath)
  }

  def file: RIO[LocalFile] =
    fileWithContent("")

  def pathWithParent: RIO[LocalPath] = for {
    f <- path
    _ <- f.dirname.mkdirs
  } yield f

  def fileWithContent(content: String): RIO[LocalFile] = for {
    f <- path
    r <- f.write(content)
  } yield r

  def directory: RIO[LocalDirectory] = {
    val path = setup
    path.mkdirs >>= (d =>
      run(s"LocalDirectory($path.path)") >>
        RIO.ok(d))
  }

  def setup: LocalPath = {
    val incr = step.incrementAndGet.toString
    val path = base /- seed /- incr
    path
  }

  def run(msg: String): RIO[Unit] =
    addCleanupFinalizer(msg) >>
      addPrintFinalizer(msg)

  def addCleanupFinalizer(str: String): RIO[Unit] =
    if (skipCleanup) forceAddPrintFinalizer(str)
    else             RIO.addFinalizer(Finalizer(base.delete))

  def addPrintFinalizer(str: String): RIO[Unit] =
    if (print && !skipCleanup) forceAddPrintFinalizer(str)
    else                       RIO.unit

  def forceAddPrintFinalizer(str: String): RIO[Unit] =
    RIO.addFinalizer(Finalizer(RIO.putStrLn(s"LocalTemporary: $str")))
}

object LocalTemporary {
  def random: LocalTemporary =
    LocalTemporary(uniqueLocalPath, s"temporary-${java.util.UUID.randomUUID().toString}/")
}
