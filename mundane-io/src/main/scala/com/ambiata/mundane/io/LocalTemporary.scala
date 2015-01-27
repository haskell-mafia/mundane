package com.ambiata.mundane.io

import com.ambiata.mundane.control._
import com.ambiata.mundane.io.Temporary._
import java.util.concurrent.atomic.AtomicInteger
import scalaz._, Scalaz._

case class LocalTemporary(seed: String) {
  private val step: AtomicInteger = new AtomicInteger(0)

  def path: RIO[LocalPath] = {
    val (base, path) = setup
    val filePath = path /- java.util.UUID.randomUUID().toString
    run(base, s"LocalPath($filePath.path)") >>
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
    val (base, path) = setup
    path.mkdirs >>= (d =>
      run(base, s"LocalDirectory($path.path)") >>
        RIO.ok(d))
  }

  /** (base, path) */
  def setup: (LocalPath, LocalPath) = {
    val base = uniqueLocalPath
    val incr = step.incrementAndGet.toString
    val path = base /- incr /- seed
    (base, path)
  }

  def run(base: LocalPath, msg: String): RIO[Unit] =
    addCleanupFinalizer(base, msg) >>
      addPrintFinalizer(msg)

  def addCleanupFinalizer(path: LocalPath, str: String): RIO[Unit] =
    if (skipCleanup) forceAddPrintFinalizer(str)
    else             RIO.addFinalizer(Finalizer(path.delete))

  def addPrintFinalizer(str: String): RIO[Unit] =
    if (print && !skipCleanup) forceAddPrintFinalizer(str)
    else                       RIO.unit

  def forceAddPrintFinalizer(str: String): RIO[Unit] =
    RIO.addFinalizer(Finalizer(RIO.putStrLn(s"Temporary: $str")))
}

object LocalTemporary {
  def random: LocalTemporary =
    LocalTemporary(s"temporary-${java.util.UUID.randomUUID().toString}/")
}
