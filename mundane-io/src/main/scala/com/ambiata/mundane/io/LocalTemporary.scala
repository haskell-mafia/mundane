package com.ambiata.mundane.io

import com.ambiata.mundane.control._
import com.ambiata.mundane.io.Temporary._
import java.util.concurrent.atomic.AtomicInteger
import scalaz._, Scalaz._

case class LocalTemporary(seed: String) {
  private val step: AtomicInteger = new AtomicInteger(0)

  def file: RIO[FilePath] = {
    val (base, path) = setup
    val filePath = path </> FilePath.unsafe(java.util.UUID.randomUUID().toString)
    run(base, s"File($filePath.path)") >>
      RIO.ok(filePath)
  }

  def fileThatExists: RIO[FilePath] =
    fileWithContent("")

  def fileWithParent: RIO[FilePath] =
    file.flatMap(f => Directories.mkdirs(f.dirname).as(f))

  def fileWithContent(content: String): RIO[FilePath] = for {
    f <- file
    _ <- Files.write(f, content)
  } yield f

  def directory: RIO[DirPath] = {
    val (base, path) = setup
    run(base, s"Directory($path.path)") >>
      RIO.ok(path)
  }

  def directoryThatExists: RIO[DirPath] = for {
    d <- directory
    _ <- Directories.mkdirs(d)
  } yield d

  /** (base, path) */
  def setup: (DirPath, DirPath) = {
    val base = uniqueDirPath
    val incr = step.incrementAndGet.toString
    val path = base </> DirPath.unsafe(incr) </> DirPath.unsafe(seed)
    (base, path)
  }

  def run(base: DirPath, msg: String): RIO[Unit] =
    addCleanupFinalizer(base, msg) >>
      addPrintFinalizer(msg)

  def addCleanupFinalizer(path: DirPath, str: String): RIO[Unit] =
    if (skipCleanup) forceAddPrintFinalizer(str)
    else             RIO.addFinalizer(Finalizer(Directories.delete(path).void))

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
