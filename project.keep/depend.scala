import sbt._
import Keys._

object depend {
  val scalaz = Seq(  "org.scalaz"           %% "scalaz-core"    % "7.0.6"
                   , "org.scalaz"           %% "scalaz-effect"  % "7.0.6")
  val scopt  = Seq(  "com.github.scopt"     %% "scopt"          % "3.1.0")
  val joda   = Seq(  "joda-time"            %  "joda-time"      % "2.1"
                   , "org.joda"             %  "joda-convert"   % "1.1")
  val bits   = Seq(  "org.typelevel"        %% "scodec-bits"    % "1.0.0")
  val stream = Seq(  "org.scalaz.stream"    %% "scalaz-stream"  % "0.3.1")
  val specs2 = Seq(  "org.specs2"           %% "specs2-core"
                   , "org.specs2"           %% "specs2-junit"
                   , "org.specs2"           %% "specs2-scalacheck").map(_ % "2.3.10")

  val parboiled = Seq("org.parboiled"       %% "parboiled"      % "2.0-M2")

  val scrutiny = Seq("com.ambiata"          %% "scrutiny"       % "1.1-20140316092635-8dfec97" % "test")

  val rng =      Seq("com.nicta"            %% "rng"            % "1.2.1")

  val kiama =    Seq("com.googlecode.kiama" %% "kiama"          % "1.5.1")

  val resolvers = Seq(
      Resolver.sonatypeRepo("releases")
    , Resolver.typesafeRepo("releases")
    , "artifactory"           at "http://etd-packaging.research.nicta.com.au/artifactory/libs-release-local"
    , "Scalaz Bintray Repo"   at "http://dl.bintray.com/scalaz/releases")
}
