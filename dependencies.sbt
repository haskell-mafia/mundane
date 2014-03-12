libraryDependencies ++= Seq(
    "com.nicta"              %% "rng"                % "1.2.1",
    "joda-time"              %  "joda-time"          % "2.1",
    "org.joda"               %  "joda-convert"       % "1.1",
    "com.github.scopt"       %% "scopt"              % "3.1.0",
    "com.googlecode.kiama"   %% "kiama"              % "1.5.1",
    "commons-io"             %  "commons-io"         % "2.4",
    "org.parboiled"          %% "parboiled"          % "2.0-M2",
    "org.specs2"             %% "specs2-matcher"     % "2.3.10" % "optional",
    "org.scalacheck"         %% "scalacheck"         % "1.11.1" % "optional"
  )

libraryDependencies ++= 
  Seq(
    "com.ambiata"         %% "scrutiny"             % "1.1-20140122014104-1b0d7e9" % "test") ++
  Seq(
    "org.specs2"          %% "specs2-matcher-extra",
    "org.specs2"          %% "specs2-core"         ,
    "org.specs2"          %% "specs2-junit"        ,
    "org.specs2"          %% "specs2-scalacheck"   ).map(_ % "2.3.10" % "test")


resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.typesafeRepo("releases"),
  "artifactory"           at "http://etd-packaging.research.nicta.com.au/artifactory/libs-release-local")
