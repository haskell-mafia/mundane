import sbt._
import Keys._
import com.ambiata.promulgate.Plugin._

object build extends Build {
  type Settings = Def.Setting[_]

  lazy val mundane = Project(
      id = "mundane"
    , base = file(".")
    , settings = standardSettings
    , aggregate = Seq(cli, control, data, error, io, parse, reflect, testing, time)
    )
    .dependsOn(cli, control, data, error, io, parse, reflect, testing, time)

  lazy val standardSettings = Defaults.defaultSettings ++
                   projectSettings          ++
                   compilationSettings      ++
                   testingSettings          ++
                   publishingSettings       ++
                   packageSettings

  lazy val projectSettings: Seq[Settings] = Seq(
      name := "mundane"
    , version in ThisBuild := "1.2.1"
    , organization := "com.ambiata"
    , scalaVersion := "2.10.4"
  ) ++ Seq(prompt)

  lazy val cli = Project(
    id = "cli"
  , base = file("mundane-cli")
  , settings = standardSettings ++ Seq[Settings](
      name := "mundane-cli"
    ) ++ Seq[Settings](libraryDependencies ++= depend.scopt ++ depend.scalaz ++ depend.joda)
  )

  lazy val control = Project(
    id = "control"
  , base = file("mundane-control")
  , settings = standardSettings ++ Seq[Settings](
      name := "mundane-control"
    ) ++ Seq[Settings](libraryDependencies ++= depend.scalaz ++ depend.specs2)
  )
  .dependsOn(error)

  lazy val daemon = Project(
    id = "daemon"
  , base = file("mundane-daemon")
  , settings = standardSettings ++ Seq[Settings](
      name := "mundane-daemon"
    ) ++ Seq[Settings](libraryDependencies ++= depend.scalaz ++ depend.specs2 ++ depend.scrutiny)
  )
  .dependsOn(control, io)

  lazy val data = Project(
    id = "data"
  , base = file("mundane-data")
  , settings = standardSettings ++ Seq[Settings](
      name := "mundane-data"
    ) ++ Seq[Settings](libraryDependencies ++= depend.rng ++ depend.specs2 ++ depend.kiama)
  )

  lazy val error = Project(
    id = "error"
  , base = file("mundane-error")
  , settings = standardSettings ++ Seq[Settings](
      name := "mundane-error"
    )
  )

  lazy val io = Project(
    id = "io"
  , base = file("mundane-io")
  , settings = standardSettings ++ Seq[Settings](
      name := "mundane-io"
    ) ++ Seq[Settings](libraryDependencies ++= depend.scalaz ++ depend.joda ++ depend.specs2 ++ depend.scrutiny)
  )
  .dependsOn(control, data)

  lazy val parse = Project(
    id = "parse"
  , base = file("mundane-parse")
  , settings = standardSettings ++ Seq[Settings](
      name := "mundane-parse"
    ) ++ Seq[Settings](libraryDependencies ++= depend.parboiled ++ depend.joda)
  )
  .dependsOn(control)

  lazy val reflect = Project(
    id = "reflect"
  , base = file("mundane-reflect")
  , settings = standardSettings ++ Seq[Settings](
      name := "mundane-reflect"
    )
  )

  lazy val testing = Project(
    id = "testing"
  , base = file("mundane-testing")
  , settings = standardSettings ++ Seq[Settings](
      name := "mundane-testing"
    ) ++ Seq[Settings](libraryDependencies ++= depend.specs2 ++ depend.scrutiny)
  )
  .dependsOn(control, io)

  lazy val time = Project(
    id = "time"
  , base = file("mundane-time")
  , settings = standardSettings ++ Seq[Settings](
      name := "mundane-time"
    ) ++ Seq[Settings](libraryDependencies ++= depend.scalaz ++ depend.joda ++ depend.specs2)
  )

  lazy val compilationSettings: Seq[Settings] = Seq(
    javacOptions ++= Seq("-Xmx3G", "-Xms512m", "-Xss4m"),
    maxErrors := 20,
    // incOptions := incOptions.value.withNameHashing(true),
    scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-language:_", "-Ywarn-all", "-Xlint"),
    scalacOptions in Test ++= Seq("-Yrangepos")
  )

  lazy val packageSettings: Seq[Settings] = promulgate.library ++ Seq(
    promulgate.pkg := "com.ambiata.mundane"
  )

  lazy val testingSettings: Seq[Settings] = Seq(
    initialCommands in console := "import org.specs2._",
    logBuffered := false,
    cancelable := true,
    javaOptions += "-Xmx3G"
  )

  lazy val publishingSettings: Seq[Settings] = Seq(
    publishMavenStyle := true,
    publishArtifact in Test := false,
    pomIncludeRepository := { _ => false },
    publishTo <<= version { v =>
      val artifactory = "http://etd-packaging.research.nicta.com.au/artifactory/"
      val flavour = if (v.trim.endsWith("SNAPSHOT")) "libs-snapshot-local" else "libs-release-local"
      val url = artifactory + flavour
      val name = "etd-packaging.research.nicta.com.au"
      Some(Resolver.url(name, new URL(url)))
    },
    credentials += Credentials(Path.userHome / ".credentials")
  )

  lazy val prompt = shellPrompt in ThisBuild := { state =>
    val name = Project.extract(state).currentRef.project
    (if (name == "mundane") "" else name) + "> "
  }

}
