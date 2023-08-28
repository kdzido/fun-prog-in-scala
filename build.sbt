ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.2"

lazy val scalatestVersion = "3.2.16"

lazy val root = (project in file("."))
  .settings(
    name := "fun-prog-in-scala",
    libraryDependencies += "org.openjdk.jol" % "jol-core" % "0.17",

    libraryDependencies += "org.scalactic" %% "scalactic" % scalatestVersion,
    libraryDependencies += "org.scalatest" %% "scalatest" % scalatestVersion % Test
  )
