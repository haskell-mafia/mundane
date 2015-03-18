package com.ambiata.mundane.ssh

import java.security.PublicKey

import com.ambiata.mundane.control._
import com.ambiata.mundane.io._
import com.ambiata.mundane.testing.RIOMatcher._

import org.apache.sshd.SshServer
import org.apache.sshd.server.{CommandFactory, PublickeyAuthenticator}
import org.apache.sshd.server.command.ScpCommandFactory
import org.apache.sshd.server.keyprovider.PEMGeneratorHostKeyProvider
import org.apache.sshd.server.session.ServerSession
import org.apache.sshd.server.shell.ProcessShellFactory

import org.specs2._

class SshActionSpec extends Specification with ScalaCheck { def is = s2"""

  Can upload and download a file
    $upload

  Will return None on missing download
    $downloadMissing

  Can upload and download a string
    $heredoc

  Can exec a command
    $exec

  Will fail when trying to exec and missing command
    $execMissing
"""

  def upload = prop((n: String, b: Array[Byte]) => {
    run(for {
      p <- SshAction.fromRIO(LocalTemporary.random.fileWithParent)
      _ <- SshAction.upload(n, b, p.path)
      r <- SshAction.download(p.path)
    } yield r.map(_.toList) ==== Some(b.toList))
  }).set(minTestsOk = 3)

  def downloadMissing =
    run(for {
      p <- SshAction.fromRIO(LocalTemporary.random.fileWithParent)
      b <- SshAction.download(p.path)
    } yield b ==== None)

  def heredoc = prop((s: String) => {
    run(for {
      p <- SshAction.fromRIO(LocalTemporary.random.fileWithParent)
      _ <- SshAction.heredoc(s, p.path)
      r <- SshAction.readUtf8(p.path)
    } yield r==== Some(s))
  }).set(minTestsOk = 3)

  def exec =
    run(SshAction.exec("echo").isSuccess)

  def execMissing =
    run(SshAction.exec("bad").isSuccess.softFail.map(_ ==== false))

  def run[A](f: SshAction[A]): RIO[A] = {
    val port = randomPort
    val user = java.util.UUID.randomUUID.toString
    val keyFile = java.io.File.createTempFile(user, ".pem").getAbsolutePath
    val sshd = SshServer.setUpDefaultServer
    sshd.setPort(port)
    sshd.setKeyPairProvider(new PEMGeneratorHostKeyProvider(keyFile))
    sshd.setCommandFactory(new ScpCommandFactory(new CommandFactory {
      def createCommand(command: String) =
        new ProcessShellFactory(command.split(" ")).create()
    }))
    sshd.setPublickeyAuthenticator(new PublickeyAuthenticator {
      def authenticate(username: String, key: PublicKey, session: ServerSession): Boolean =
        username == user
    })
    sshd.start
    f.runWith(user, keyFile, "localhost", port, SshConfiguration.defaultDontVerify)
      .ensuring(RIO.io(sshd.close(true)))
      .ensuring(Files.delete(FilePath.unsafe(keyFile)))
  }

  def randomPort: Int =
    1080 + new java.util.Random().nextInt(10000)
}
