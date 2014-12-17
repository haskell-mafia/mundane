package com.ambiata.mundane.io

import com.ambiata.mundane.control._
import com.ambiata.mundane.path._
import MemoryConversions._

import scalaz._, Scalaz._
import scalaz.effect._

/**
 * Functions acting on directories (local filesystem):
 *
 *  - mkdirs: create directories
 *  - list: list files in a directory
 *  - delete: delete a directory
 *  - size: size of a directory
 *  - ...
 */
object Directories {
  def mkdirs(dirPath: LocalDirectory): RIO[Unit] =
    RIO.safe[Boolean](dirPath.toFile.mkdirs).void

  def list(dirPath: LocalDirectory): RIO[List[FilePath]] = RIO.safe[List[FilePath]] {
    def loop(dir: LocalDirectory): Vector[LocalFile] = {
      val files = Option(dir.toFile.listFiles).cata(_.toVector, Vector())
      files.flatMap { f =>
        if (f.isDirectory) loop(dir </ FileName.unsafe(f.getName))
        else               Vector(dir </ FileName.unsafe(f.getName))
      }
    }
    loop(dirPath).toList
  }

  def listFirstFileNames(dirPath: LocalDirectory): RIO[List[FileName]] =
    RIO.safe[List[FileName]](Option(dirPath.toFile.listFiles).cata(_.toList, List()).map(file => FileName.unsafe(file.getName)))

  def delete(dirPath: LocalDirectory): RIO[Boolean] = RIO.safe[Boolean] {
    def loop(dir: LocalDirectory): Boolean = {
      val files = Option(dir.toFile.listFiles).cata(_.toVector, Vector())
      files.forall { f =>
        if (f.isDirectory) loop(dir </ FileName.unsafe(f.getName))
        else               f.delete
      } && dir.toFile.delete
    }
    loop(dirPath)
  }

  def move(src: LocalDirectory, dest: LocalDirectory): RIO[Unit] = RIO.safe {
    val destf = dest.toFile
    destf.mkdirs
    src.toFile.renameTo(destf)
  }

  def exists(dirPath: LocalDirectory): RIO[Boolean] = RIO.safe[Boolean] {
    val file = dirPath.toFile
    file.exists && file.isDirectory
  }

  def size(dirPath: LocalDirectory): RIO[BytesQuantity] =
    list(dirPath).map(_.foldMap(_.toFile.length).bytes)
}
