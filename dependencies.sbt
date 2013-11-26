libraryDependencies ++= Seq(
    "com.nicta"              %% "rng"                % "1.0-SNAPSHOT",
    "org.scalaz"             %% "scalaz-core"        % "7.0.4",
    "joda-time"              %  "joda-time"          % "2.1",
    "org.joda"               %  "joda-convert"       % "1.1",
    "com.googlecode.kiama"   %% "kiama"              % "1.5.1",
    "commons-io"             %  "commons-io"         % "2.4"
  )

libraryDependencies ++= Seq(
    "org.specs2"          %% "specs2-core"        % "2.3.4"        % "test",
    "org.specs2"          %% "specs2-scalacheck"  % "2.3.4"        % "test",
    "org.scalacheck"      %% "scalacheck"         % "1.11.1"       % "test")


resolvers ++=
  Seq("snapshots", "releases").map(Resolver.sonatypeRepo) ++
  Seq(
    Resolver.typesafeRepo("releases"),
    "cloudera" at "https://repository.cloudera.com/content/repositories/releases")
