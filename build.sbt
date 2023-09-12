ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.2.2"

lazy val scalatestVersion = "3.2.16"
lazy val commonSettings = Seq(
  libraryDependencies ++= Seq(
    "org.scalactic" %% "scalactic" % scalatestVersion,
    "org.scalatest" %% "scalatest" % scalatestVersion % Test
  )
)

lazy val chapter01 = (project in file("chapter01"))
  .settings(commonSettings)

lazy val chapter02 = (project in file("chapter02"))
  .settings(commonSettings)

lazy val chapter03 = (project in file("chapter03"))
  .settings(commonSettings)

lazy val chapter04 = (project in file("chapter04"))
  .settings(commonSettings)

lazy val chapter05 = (project in file("chapter05"))
  .settings(commonSettings)

lazy val chapter06 = (project in file("chapter06"))
  .settings(commonSettings)

lazy val root = (project in file("."))
  .settings(
    name := "fp-in-scala-book",
    commonSettings,
    libraryDependencies += "org.openjdk.jol" % "jol-core" % "0.17",
  )
  .aggregate(chapter01, chapter02, chapter03, chapter04, chapter05, chapter06)
