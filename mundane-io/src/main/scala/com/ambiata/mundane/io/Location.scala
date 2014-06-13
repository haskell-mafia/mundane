package com.ambiata.mundane.io

import scalaz._, Scalaz._

sealed trait Location {
  def path: FilePath
}
case class HdfsLocation(uriPath: String) extends Location {
  def path: FilePath = FilePath(uriPath)
}
case class S3Location(bucket: String, key: String) extends Location {
  def path: FilePath = bucket </> key
}
case class LocalLocation(filePath: String) extends Location {
  def path: FilePath = FilePath(filePath)
}

object Location {
  def fromUri(s: String): String \/ Location = try {
    val uri = new java.net.URI(s)
    uri.getScheme match {
      case "hdfs" =>
        HdfsLocation(uri.getPath).right
      case "s3" =>
        S3Location(uri.getHost, uri.getPath.drop(1)).right
      case "file" =>
        LocalLocation(uri.toURL.getFile).right
      case null =>
        LocalLocation(uri.getPath).right
      case _ =>
        s"Unknown or invalid repository scheme [${uri.getScheme}]".left
    }
  } catch {
    case e: java.net.URISyntaxException =>
      e.getMessage.left
  }
}

