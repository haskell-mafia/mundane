package com.ambiata.mundane.ssh

import com.decodified.scalassh.HostKeyVerifiers._

import net.schmizz.sshj.transport.verification.HostKeyVerifier

case class SshConfiguration(
    hostKeyVerifier: HostKeyVerifier
  , connectTimeout: Int
  , connectionTimeout: Int
  , commandTimeout: Int
  )

object SshConfiguration {

  def defaultDontVerify: SshConfiguration =
    SshConfiguration(
      hostKeyVerifier = DontVerify,
      connectTimeout = 10 * 1000,
      connectionTimeout = Int.MaxValue,
      commandTimeout = Int.MaxValue
    )
}
