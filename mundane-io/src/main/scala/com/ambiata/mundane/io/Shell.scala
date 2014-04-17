package com.ambiata.mundane
package io

import control._
import ActionT._
import scalaz._, Scalaz._
import scala.sys.process._
import java.io.File

/**
 * Run a command: ls or a script, with some given environment variables
 */
trait Shell {

  /**
   * execute a shell command
   */
  def execute(cmd: String, env: Env, arguments: Seq[String] = Seq(), verbose: Boolean = false, commandType: Option[String] = None): IOAction[String] =
    attempt(cmd, env, arguments, verbose, commandType).flatMap({
      case (returnValue, out, err) =>
        val dud = makeErrorMessage(cmd, commandType, err.mkString("\n"), returnValue, env)
        if (returnValue == 0) IOActions.ok(out.mkString("\n") + (if (err.isEmpty) "" else dud))
        else                  IOActions.fail(dud)
    })

  /**
   * execute a shell command
   */
  def attempt(cmd: String, env: Env, arguments: Seq[String] = Seq(), verbose: Boolean = false, commandType: Option[String] = None): IOAction[(Int, List[String], List[String])] =
    for {
      _ <- if (verbose) log(s"""${commandType.map(ct => s"[$ct]").getOrElse("")} executing command '$cmd'""") else IOActions.ok(())
      r <- IOActions.result { logger =>
        val resultOut = new scala.collection.mutable.ListBuffer[String]
        val resultErr = new scala.collection.mutable.ListBuffer[String]
        val processLogger = ProcessLogger(out => { logger(out).unsafePerformIO; resultOut.append(out) },
          err => { logger(err).unsafePerformIO; resultErr.append(err) })

        val returnValue = Process(Seq("sh", "-c", cmd + arguments.mkString(" ", " ", "")), None, env.toSeq:_*) ! processLogger
        Result.ok((returnValue, resultOut.toList, resultErr.toList))
      }
    } yield r

  private def makeErrorMessage(cmd: String, commandType: Option[String], err: String, returnValue: Int, env: Env): String = {
    val errString    = if (err.trim.nonEmpty) s"\n the error is $err\n" else ""
    val returnString = if (returnValue != 0) ", the return value is "+returnValue else ""
    val content      = if (new File(cmd).exists)
      "\n\n================"+
        "script content\n\n"+
        scriptFileContent(cmd, env)+
        "\n\n================\n\n" else ""

    s"can not execute${commandType.map(" "+_).getOrElse("")}: $cmd\n" + returnString + errString + content
  }

  /**
   * read the content of a script file to report it in case of an error
   */
  private def scriptFileContent(path: String, env: Env): String = {
    val lines = scala.io.Source.fromFile(path).getLines.mkString("\n")
    env.foldLeft(lines) { case (res, (key, value)) =>
      res.replace("${"+key+"}", value).
        replace("$"+key+"", value)
    }
  }

  /**
   * execute a shell command remotely
   */
  def executeRemotely(command: String, env: Env, remote: Remote, verbose: Boolean = false, commandType: Option[String] = None): IOAction[String] =
    execute(s"ssh ${remote.remoteKey} -p ${remote.remotePort} ${remote.remoteUser}${remote.remoteHost} '$command'", env, Seq(), verbose, commandType)

  /**
   * attempt a shell command remotely
   */
  def attemptRemotely(command: String, env: Env, remote: Remote, verbose: Boolean = false, commandType: Option[String] = None): IOAction[(Int, List[String], List[String])] =
    attempt(s"ssh ${remote.remoteKey} -p ${remote.remotePort} ${remote.remoteUser}${remote.remoteHost} '$command'", env, Seq(), verbose, commandType)

  /** upload a file to a remote server */
  def upload(file: File, destination: String, env: Env, remote: Remote, verbose: Boolean = false, commandType: Option[String] = None): IOAction[String] =
    execute(s"scp ${remote.remoteKey} -P ${remote.remotePort} ${file.getPath} ${remote.remoteUser}${remote.remoteHost}:$destination", env, Seq(), verbose, commandType)

}

object Shell extends Shell

/**
 * options for running remotely
 */
case class Remote(host: Option[String] = None, user: Option[String] = None, key: Option[String] = None, port: Option[Int] = None) {
  def setHost(h: String) = copy(host = Some(h))
  def setUser(u: String) = copy(user = Some(u))
  def setKey(k: String)  = copy(key  = Some(k))
  def setPort(p: Int)    = copy(port = Some(p))

  def isDefined = host.isDefined

  def remoteUser = user.map(_ + "@").getOrElse("")
  def remoteHost = host.getOrElse("local")
  def remoteKey  = key.map("-i " + _).getOrElse("")
  def remotePort = port.getOrElse(22)

  override def toString =
    s"host $remoteHost (user=$remoteUser, key=$remoteKey, port=$remotePort)"
}
