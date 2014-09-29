package com.ambiata.mundane.io

import java.net.URI

import scalaz._, Scalaz._

sealed trait Location {
  def path: DirPath
  def uri: URI
}

case class HdfsLocation (path: DirPath, uri: URI) extends Location
case class S3Location   (path: DirPath, uri: URI) extends Location {
  def bucket: String = path.rootname.basename.name
  def key: String = path.fromRoot.path
}
case class LocalLocation(path: DirPath, uri: URI) extends Location

object Location {
  def fromUri(s: String): String \/ Location = try {
    val uri = new java.net.URI(s)
    val dirPath = DirPath.unsafe(s)

    uri.getScheme match {
      case "hdfs" => HdfsLocation (dirPath, uri).right
      case "s3"   => S3Location   (dirPath, uri).right
      case "file" => LocalLocation(dirPath, uri).right
      case null   => LocalLocation(dirPath, uri).right
      case _      => s"Unknown or invalid repository scheme [${uri.getScheme}]".left
    }
  } catch {
    case e: java.net.URISyntaxException => e.getMessage.left
  }

  def unapply(location: Location): Option[DirPath] =
    Some(location.path)

}

