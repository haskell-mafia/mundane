import sbt._
import Keys._

import com.ambiata.promulgate.project.ProjectPlugin._
import scoverage.ScoverageSbtPlugin._

object build extends Build {
  type Settings = Def.Setting[_]

  lazy val mundane = Project(
      id = "mundane"
    , base = file(".")
    , settings = standardSettings ++ lib("com.ambiata.mundane")
    , aggregate = Seq(bytes, cli, control, csv, data, error, io, parse, reflect, testing, testingExtra, time, trace, path)
    )
    .dependsOn(bytes, cli, control, data, csv, error, io, parse, reflect, testing, time, trace, path)

  lazy val standardSettings = Defaults.coreDefaultSettings ++
                              projectSettings              ++
                              compilationSettings          ++
                              testingSettings              ++
                              Seq(resolvers ++= depend.resolvers)

  lazy val projectSettings: Seq[Settings] = Seq(
      name := "mundane"
    , version in ThisBuild := "1.2.1"
    , organization := "com.ambiata"
    , scalaVersion := "2.11.2"
    , crossScalaVersions := Seq(scalaVersion.value)
    , fork in run  := true
    // https://gist.github.com/djspiewak/976cd8ac65e20e136f05
    , unmanagedSourceDirectories in Compile += (sourceDirectory in Compile).value / s"scala-${scalaBinaryVersion.value}"
    , updateOptions := updateOptions.value.withCachedResolution(true)
    , publishArtifact in (Test, packageBin) := true
  ) ++ Seq(prompt)

  lazy val bytes = Project(
    id = "bytes"
  , base = file("mundane-bytes")
  , settings = standardSettings ++ lib("bytes") ++ Seq[Settings](
      name := "mundane-bytes"
    ) ++ Seq[Settings](libraryDependencies ++= depend.scalaz ++ depend.specs2 ++ depend.disorder)
  )

  lazy val cli = Project(
    id = "cli"
  , base = file("mundane-cli")
  , settings = standardSettings ++ lib("cli") ++ Seq[Settings](
      name := "mundane-cli"
    ) ++ Seq[Settings](libraryDependencies ++= depend.scopt ++ depend.scalaz ++ depend.joda)
  )

  lazy val control = Project(
    id = "control"
  , base = file("mundane-control")
  , settings = standardSettings ++ lib("control") ++ Seq[Settings](
      name := "mundane-control"
    ) ++ Seq[Settings](libraryDependencies ++= depend.scalaz ++ depend.specs2)
  )
  .dependsOn(error)

  lazy val data = Project(
    id = "data"
  , base = file("mundane-data")
  , settings = standardSettings ++ lib("data") ++ Seq[Settings](
      name := "mundane-data"
    ) ++ Seq[Settings](libraryDependencies ++= depend.scalaz ++ depend.rng ++ depend.specs2 ++ depend.kiama)
  )

  lazy val csv = Project(
    id = "csv"
    , base = file("mundane-csv")
    , settings = standardSettings ++ lib("csv") ++ Seq[Settings](
      name := "mundane-csv"
    ) ++ Seq[Settings](libraryDependencies <++= scalaVersion(sv => depend.simpleCsv ++ depend.parboiled(sv) ++ depend.specs2 ++ depend.disorder ++ depend.caliper)) ++
      // enable forking in run
    Seq(fork in run := true,
    // we need to add the runtime classpath as a "-cp" argument to the `javaOptions in run`, otherwise caliper
    // will not see the right classpath and die with a ConfigurationException
    javaOptions in run ++= Seq("-cp", Attributed.data((fullClasspath in Runtime).value).mkString(":")))
  ).dependsOn(control)

  lazy val error = Project(
    id = "error"
  , base = file("mundane-error")
  , settings = standardSettings ++ lib("error") ++ Seq[Settings](
      name := "mundane-error"
    )
  )

  lazy val io = Project(
    id = "io"
  , base = file("mundane-io")
  , settings = standardSettings ++ lib("io") ++ Seq[Settings](
      name := "mundane-io"
    ) ++ Seq[Settings](
      libraryDependencies ++= depend.scalaz ++ depend.joda ++ depend.specs2 ++
                              depend.reflect(scalaVersion.value) ++ depend.disorder
    )
  )
  .dependsOn(control, data, reflect, path, path % "test->test", testing % "test->test")

  lazy val path = Project(
    id = "path"
  , base = file("mundane-path")
  , settings = standardSettings ++ lib("path") ++ Seq[Settings](
      name := "mundane-path"
    ) ++ Seq[Settings](
      libraryDependencies ++= depend.scalaz ++ depend.joda ++ depend.testing ++
                              depend.reflect(scalaVersion.value)
    )
  )
  .dependsOn(control, reflect, testing % "test")

  lazy val parse = Project(
    id = "parse"
  , base = file("mundane-parse")
  , settings = standardSettings ++ lib("parse") ++ Seq[Settings](
      name := "mundane-parse"
    ) ++ Seq[Settings](libraryDependencies <++= scalaVersion(sv => depend.parboiled(sv) ++ depend.joda ++ depend.specs2))
  )
  .dependsOn(control, csv)

  lazy val reflect = Project(
    id = "reflect"
  , base = file("mundane-reflect")
  , settings = standardSettings ++ lib("reflect") ++ Seq[Settings](
        name := "mundane-reflect"
      , libraryDependencies ++= depend.reflect(scalaVersion.value) ++ depend.specs2
    )
  )

  lazy val store = Project(
    id = "store"
  , base = file("mundane-store")
  , settings = standardSettings ++ lib("store") ++ Seq[Settings](
      name := "mundane-store"
    ) ++ Seq[Settings](libraryDependencies ++= depend.scalaz ++ depend.specs2 ++ depend.bits ++ depend.stream)
  )
  .dependsOn(control, data, io, testing % "test->test", io % "test->test")

  lazy val testing = Project(
    id = "testing"
  , base = file("mundane-testing")
  , settings = standardSettings ++ lib("testing") ++ Seq[Settings](
      name := "mundane-testing"
    ) ++ Seq[Settings](libraryDependencies ++= depend.specs2)
  )
  .dependsOn(control, parse)

  lazy val testingExtra = Project(
    id = "testing-extra"
    , base = file("mundane-testing-extra")
    , settings = standardSettings ++ lib("testing") ++ Seq[Settings](
      name := "mundane-testing-extra"
    ) ++ Seq[Settings](libraryDependencies ++= depend.specs2 ++ depend.specs2Extra)
  )

  lazy val time = Project(
    id = "time"
  , base = file("mundane-time")
  , settings = standardSettings ++ lib("time") ++ Seq[Settings](
      name := "mundane-time"
    ) ++ Seq[Settings](libraryDependencies ++= depend.scalaz ++ depend.joda ++ depend.specs2)
  ).dependsOn(data)

  lazy val trace = Project(
    id = "trace"
  , base = file("mundane-trace")
  , settings = standardSettings ++ lib("trace") ++ Seq[Settings](
      name := "mundane-trace"
    ) ++ Seq[Settings](libraryDependencies ++= depend.scalaz ++ depend.specs2)
  )
  .dependsOn(data, control, io, testing % "test->test")

  lazy val compilationSettings: Seq[Settings] = Seq(
    javacOptions ++= Seq("-Xmx3G", "-Xms512m", "-Xss4m"),
    maxErrors := 10,
    scalacOptions <++= scalaVersion.map({
      case x if x.contains("2.11") => Seq("-deprecation", "-unchecked", "-feature", "-language:_", "-Xlint")
      case x if x.contains("2.10") => Seq("-deprecation", "-unchecked", "-feature", "-language:_", "-Ywarn-all", "-Xlint")
      case x => sys.error("Unsupported scala version: " + x)
    }),
    scalacOptions in Test ++= Seq("-Yrangepos"),
    scalacOptions in ScoverageCompile := Seq("-language:_", "-feature")
  )

  lazy val ossBucket: String = 
    sys.env.getOrElse("AMBIATA_IVY_OSS", "ambiata-oss")

  def lib(name: String): Seq[Settings] =
    promulgate.library(s"com.ambiata.mundane.$name", ossBucket)

  lazy val testingSettings: Seq[Settings] = Seq(
    initialCommands in (Test, console) := "import org.specs2._",
    logBuffered := false,
    cancelable := true,
    javaOptions += "-Xmx3G"
  ) ++ instrumentSettings ++ Seq(ScoverageKeys.highlighting := true)

  lazy val prompt = shellPrompt in ThisBuild := { state =>
    val name = Project.extract(state).currentRef.project
    (if (name == "mundane") "" else name) + "> "
  }

}
