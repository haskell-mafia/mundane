package com.ambiata.mundane.io

import scalaz._, Scalaz._

sealed trait Location {
  def path: DirPath
}
case class HdfsLocation (path: DirPath) extends Location
case class S3Location   (path: DirPath) extends Location
case class LocalLocation(path: DirPath) extends Location

object Location {
  def fromUri(s: String): String \/ Location = try {
    val uri = new java.net.URI(s)
    val dirPath = DirPath.unsafe(s)

    uri.getScheme match {
      case "hdfs" => HdfsLocation(dirPath).right
      case "s3"   => S3Location(dirPath).right
      case "file" => LocalLocation(dirPath).right
      case null   => LocalLocation(dirPath).right
      case _      => s"Unknown or invalid repository scheme [${uri.getScheme}]".left
    }
  } catch {
    case e: java.net.URISyntaxException => e.getMessage.left
  }
}

