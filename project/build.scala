import sbt._
import Keys._

import com.ambiata.promulgate.project.ProjectPlugin._

object build extends Build {
  type Settings = Def.Setting[_]

  lazy val mundane = Project(
      id = "mundane"
    , base = file(".")
    , settings = standardSettings ++ promulgate.library("com.ambiata.mundane", "ambiata-oss")
    , aggregate = Seq(cli, control, data, error, io, parse, reflect, store, testing, testingExtra, time)
    )
    .dependsOn(cli, control, data, error, io, parse, reflect, store, testing, time)

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
    , crossScalaVersions := Seq("2.10.4", scalaVersion.value)
    // https://gist.github.com/djspiewak/976cd8ac65e20e136f05
    , unmanagedSourceDirectories in Compile += (sourceDirectory in Compile).value / s"scala-${scalaBinaryVersion.value}"
  ) ++ Seq(prompt)

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
    ) ++ Seq[Settings](libraryDependencies ++= depend.scalaz ++ depend.testing)
  )
  .dependsOn(error)

  lazy val data = Project(
    id = "data"
  , base = file("mundane-data")
  , settings = standardSettings ++ lib("data") ++ Seq[Settings](
      name := "mundane-data"
    ) ++ Seq[Settings](libraryDependencies ++= depend.rng ++ depend.testing ++ depend.kiama)
  )

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
      libraryDependencies ++= depend.scalaz ++ depend.joda ++ depend.testing ++
                              depend.reflect(scalaVersion.value)
    )
  )
  .dependsOn(control, data, reflect, testing % "test")

  lazy val store = Project(
    id = "store"
  , base = file("mundane-store")
  , settings = standardSettings ++ lib("store") ++ Seq[Settings](
      name := "mundane-store"
    ) ++ Seq[Settings](libraryDependencies ++= depend.scalaz ++ depend.testing ++ depend.bits ++ depend.stream ++ depend.reflect(scalaVersion.value))
  )
  .dependsOn(control, data, io, testing % "test", io % "test->test")

  lazy val parse = Project(
    id = "parse"
  , base = file("mundane-parse")
  , settings = standardSettings ++ lib("parse") ++ Seq[Settings](
      name := "mundane-parse"
    ) ++ Seq[Settings](libraryDependencies <++= scalaVersion(sv => depend.parboiled(sv) ++ depend.joda ++ depend.testing))
  )
  .dependsOn(control)

  lazy val reflect = Project(
    id = "reflect"
  , base = file("mundane-reflect")
  , settings = standardSettings ++ lib("reflect") ++ Seq[Settings](
        name := "mundane-reflect"
      , libraryDependencies ++= depend.reflect(scalaVersion.value) ++ depend.testing
    )
  )

  lazy val testing = Project(
    id = "testing"
  , base = file("mundane-testing")
  , settings = standardSettings ++ lib("testing") ++ Seq[Settings](
      name := "mundane-testing"
    ) ++ Seq[Settings](libraryDependencies ++= depend.specs2)
  )
  .dependsOn(control)

  lazy val testingExtra = Project(
    id = "testing-extra"
    , base = file("mundane-testing-extra")
    , settings = standardSettings ++ lib("testing") ++ Seq[Settings](
      name := "mundane-testing-extra"
    ) ++ Seq[Settings](libraryDependencies ++= depend.specs2 ++ depend.specs2Extra)
  ).dependsOn(testing)

  lazy val time = Project(
    id = "time"
  , base = file("mundane-time")
  , settings = standardSettings ++ lib("time") ++ Seq[Settings](
      name := "mundane-time"
    ) ++ Seq[Settings](libraryDependencies ++= depend.scalaz ++ depend.joda ++ depend.testing)
  ).dependsOn(data)

  lazy val compilationSettings: Seq[Settings] = Seq(
    javacOptions ++= Seq("-Xmx3G", "-Xms512m", "-Xss4m"),
    maxErrors := 20,
    scalacOptions <++= scalaVersion.map({
      case x if x.contains("2.11") => Seq("-deprecation", "-unchecked", "-feature", "-language:_", "-Xlint")
      case x if x.contains("2.10") => Seq("-deprecation", "-unchecked", "-feature", "-language:_", "-Ywarn-all", "-Xlint")
      case x => sys.error("Unsupported scala version: " + x)
    }),
    scalacOptions in Test ++= Seq("-Yrangepos")
  )

  def lib(name: String) =
    promulgate.library(s"com.ambiata.mundane.$name", "ambiata-oss")

  lazy val testingSettings: Seq[Settings] = Seq(
    initialCommands in console := "import org.specs2._",
    logBuffered := false,
    cancelable := true,
    javaOptions += "-Xmx3G"
  )

  lazy val prompt = shellPrompt in ThisBuild := { state =>
    val name = Project.extract(state).currentRef.project
    (if (name == "mundane") "" else name) + "> "
  }

}
