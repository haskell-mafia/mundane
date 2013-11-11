package com.ambiata.mundane.reflect

import java.net.{URL, URLDecoder}
import scala.collection.JavaConversions._

trait Classes {
  lazy val jarName       = jarNameOption.getOrElse("jar not found for class "+getClass.getName)
  lazy val jarNameOption = findContainingJar(getClass.getClassLoader, getClass.getName)
  lazy val mainClass = getClass.getName

  /** @return the jar containing a given class */
  private def findContainingJar(loader: ClassLoader, className: String): Option[String] = {
    val resources: Seq[String] = loader.getResources(filePath(className)).toIndexedSeq.view.filter(_.getProtocol == "jar").map(filePath)
    resources.headOption
  }

  /** Return the class file path string as specified in a JAR for a given class. */
  def filePath(clazz: Class[_]): String = filePath(clazz.getName)

  /** Return the class file path string as specified in a JAR for a given class name. */
  def filePath(className: String): String = className.replaceAll("\\.", "/") + ".class"

  /** @return the file path corresponding to a full URL */
  private def filePath(url: URL): String =
    URLDecoder.decode(url.getPath.replaceAll("file:", "").replaceAll("\\+", "%2B").replaceAll("!.*$", ""), "UTF-8")
}
