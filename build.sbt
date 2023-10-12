ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.1"

ThisBuild / resolvers ++= Seq(
    Resolver.mavenLocal,
    "Gradle Releases" at "https://repo.gradle.org/gradle/libs-releases/"
)

lazy val root = (project in file("."))
  .settings(
    name := "execution-paths",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % "test",
    libraryDependencies += "io.joern" %% "javasrc2cpg" % "2.0.93",
    libraryDependencies += "io.joern" %% "joern-cli" % "2.0.93",
    libraryDependencies += "io.joern" %% "x2cpg" % "2.0.93",
    libraryDependencies += "io.joern" %% "dataflowengineoss" % "2.0.93",
    libraryDependencies += "io.joern" %% "semanticcpg" % "2.0.93",
    libraryDependencies += "io.shiftleft" %% "semanticcpg" % "1.3.522",
    libraryDependencies += "io.shiftleft" %% "codepropertygraph" % "1.4.23",
    libraryDependencies += "io.joern" %% "console" % "2.0.93"
  )
