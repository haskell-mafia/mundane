package com.ambiata.mundane.ssh

import com.ambiata.mundane.control._

import com.decodified.scalassh._
import com.decodified.scalassh.SSH.{Result => SshResult}

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}

import net.schmizz.sshj.xfer.{InMemorySourceFile, InMemoryDestFile}

import scalaz._, Scalaz._


case class SshAction[A](run: SshClient with ScpTransferable => RIO[A]) {

  def map[B](f: A => B): SshAction[B] =
    flatMap[B](f andThen SshAction.ok)

  def flatMap[B](f: A => SshAction[B]): SshAction[B] =
    SshAction[B](client => run(client).flatMap(f(_).run(client)))

  def mapError(f: \&/[String, Throwable] => \&/[String, Throwable]): SshAction[A] =
    SshAction[A](client => run(client).mapError(f))

  def onRIO[B](f: RIO[A] => RIO[B]): SshAction[B] =
    SshAction[B](client => f(run(client)))

  def runWith(user: String, key: String, address: String, port: Int, config: SshConfiguration): RIO[A] = {
    val configProvider = HostConfig(
      login = PublicKeyLogin(user, key),
      port = port,
      hostKeyVerifier = config.hostKeyVerifier,
      connectTimeout = Some(config.connectTimeout),
      connectionTimeout = Some(config.connectionTimeout),
      commandTimeout = Some(config.commandTimeout)
    )

    SshAction.validatedToRIO(SSH(address, configProvider) { inner =>
      try SshResult(run(inner).unsafePerformIO.toEither.left.map(Result.asString))
      finally inner.close
    })
  }

  def failOnError(implicit ev: A =:= CommandResult): SshAction[CommandResult] =
    flatMap(a => {
      val cr = ev(a)
      SshAction[CommandResult](_ => cr.exitCode match {
        case Some(0) => RIO.ok(cr)
        case Some(i) => RIO.fail(s"Could not execute: ${cr.stdErrAsString()}")
        case None    => RIO.fail(s"Could not get exit code: ${cr.stdErrAsString()}")
      })
    })

  def isSuccess(implicit ev: A =:= CommandResult): SshAction[Boolean] =
    failOnError.onRIO(_.isOk)

  def softFail(implicit ev: A =:= Boolean): SshAction[Boolean] =
    onRIO(_.onResult(r => Result.ok(r.fold(b => ev(b), _ => false))))

  def logError(implicit ev: A =:= CommandResult): SshAction[CommandResult] =
    flatMap(a => {
      val cr = ev(a)
      cr.exitCode match {
        case Some(0) => ()
        case Some(i) => println(s"Could not execute command: ${cr.stdErrAsString()}")
        case None    => println(s"Could not get exit code: ${cr.stdErrAsString()}")
      }
      SshAction.ok(cr)
    })
}

object SshAction {

  def ok[A](value: A): SshAction[A] =
    SshAction(_ => RIO.ok(value))

  def upload(name: String, data: Array[Byte], path: String): SshAction[Unit] =
    SshAction(c =>
      validatedToRIO(c.fileTransfer(_.upload(new InMemorySourceFile {
        def getInputStream = new ByteArrayInputStream(data)
        def getName = name
        def getLength = data.length
      }, path)).right.map(_ => ())))

  def download(path: String): SshAction[Option[Array[Byte]]] =
    SshAction(c => {
      val dest = new InMemoryDestFile {
        val stream = new ByteArrayOutputStream
        def getOutputStream = stream
      }
      RIO.safe(c.fileTransfer(_.download(path, dest))
        .fold(_ => none, _ => dest.stream.toByteArray.some))
    })

  def heredoc(content: String, path: String): SshAction[Unit] =
    upload(java.util.UUID.randomUUID.toString, content.getBytes("UTF-8"), path)

  def readUtf8(path: String): SshAction[Unit] =
    download(path).flatMap(r => SshAction.fromRIO(RIO.safe(r.map(b => new String(b, "UTF-8")))))

  def rawExec(command: String): SshAction[CommandResult] =
    SshAction(c => validatedToRIO(c.exec(command)))

  def execUnchecked(command: String): SshAction[CommandResult] =
    rawExec(command).logError

  def exec(command: String): SshAction[CommandResult] =
    rawExec(command).failOnError.mapError(_.leftMap(e => s"Command '${command} - ${e}'"))

  def validatedToRIO[A](thunk: => Validated[A]): RIO[A] =
    RIO.safe(thunk).flatMap(v => RIO.result(Result.fromEitherString(v)))

  def fromRIO[A](rio: RIO[A]): SshAction[A] =
    SshAction(_ => rio)

  implicit def SshActionMonad: Monad[SshAction] = new Monad[SshAction] {
    def point[A](v: => A) = ok(v)
    def bind[A, B](m: SshAction[A])(f: A => SshAction[B]) = m.flatMap(f)
  }
}
