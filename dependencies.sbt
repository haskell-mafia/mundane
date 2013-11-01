libraryDependencies ++= Seq(
  )

libraryDependencies ++= Seq(
    "org.specs2"          %% "specs2"             % "2.3"          % "test",
    "org.scalacheck"      %% "scalacheck"         % "1.10.0"       % "test",
    "org.hamcrest"        %  "hamcrest-all"       % "1.1"          % "test",
    "org.mockito"         %  "mockito-all"        % "1.9.5"        % "test",
    "junit"               %  "junit"              % "4.11"         % "test",
    "org.pegdown"         %  "pegdown"            % "1.2.1"        % "test",
    "org.specs2"          %  "classycle"          % "1.4.1"        % "test")


resolvers ++=
  Seq("snapshots", "releases").map(Resolver.sonatypeRepo) ++
  Seq(
    Resolver.typesafeRepo("releases"),
    "cloudera" at "https://repository.cloudera.com/content/repositories/releases")
