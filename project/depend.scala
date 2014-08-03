import sbt._
import Keys._

object depend {
  val scalaz = Seq(  "org.scalaz"           %% "scalaz-core"       % "7.1.0"
                   , "org.scalaz"           %% "scalaz-concurrent" % "7.1.0"
                   , "org.scalaz"           %% "scalaz-effect"     % "7.1.0")
  val scopt  = Seq(  "com.github.scopt"     %% "scopt"             % "3.2.0")
  val joda   = Seq(  "joda-time"            %  "joda-time"         % "2.2"
                   , "org.joda"             %  "joda-convert"      % "1.1")
  val bits   = Seq(  "org.typelevel"        %% "scodec-bits"       % "1.0.0")
  val stream = Seq(  "org.scalaz.stream"    %% "scalaz-stream"     % "0.5")
  val specs2 = Seq(  "org.specs2"           %% "specs2-core"
                   , "org.specs2"           %% "specs2-scalacheck"
                   , "org.specs2"           %% "specs2-junit").map(_ % "2.4.5")

  val specs2Extra = Seq("org.specs2"         %% "specs2-matcher-extra" % "2.4.5" excludeAll ExclusionRule(organization = "org.scalamacros"))

  val testing = specs2.map(_ % "test")

  def reflect(version: String) =
    Seq("org.scala-lang" % "scala-compiler" % version, "org.scala-lang" % "scala-reflect" % version) ++
      (if (version.contains("2.10")) Seq(
        compilerPlugin("org.scalamacros" %% "paradise" % "2.0.0" cross CrossVersion.full),
        "org.scalamacros" %% "quasiquotes" % "2.0.0") else Seq())

  def parboiled(sv: String) =
    if (sv.contains("2.11")) Seq(
        "org.scala-lang"      % "scala-reflect"   % sv
      , "org.parboiled"       %% "parboiled"      % "2.0.0"
      )
    else Seq(
        "org.parboiled"       %% "parboiled"      % "2.0.0"
      )

  val rng =      Seq("com.nicta"            %% "rng"            % "1.2.1")

  val kiama =    Seq("com.googlecode.kiama" %% "kiama"          % "1.6.0")

  val resolvers = Seq(
      Resolver.sonatypeRepo("releases")
    , Resolver.typesafeRepo("releases")
    , "Scalaz Bintray Repo"   at "http://dl.bintray.com/scalaz/releases")
}
