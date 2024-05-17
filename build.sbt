ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.2.2"

lazy val scalatestVersion = "3.2.16"
lazy val scalacheckVersion = "3.2.17.0"
lazy val commonSettings = Seq(
  libraryDependencies ++= Seq(
    "com.google.guava" % "guava" % "32.1.2-jre",
    "org.scalactic" %% "scalactic" % scalatestVersion,
    "org.scalatest" %% "scalatest" % scalatestVersion % Test,
    "org.scalatestplus" %% "scalacheck-1-17" % scalacheckVersion % Test
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

lazy val chapter07 = (project in file("chapter07"))
  .settings(commonSettings)

lazy val chapter08 = (project in file("chapter08"))
  .dependsOn(chapter06)
  .settings(commonSettings)

lazy val root = (project in file("."))
  .settings(
    name := "fp-in-scala-book",
    commonSettings,
    libraryDependencies += "org.openjdk.jol" % "jol-core" % "0.17",
  )
  .aggregate(chapter01, chapter02, chapter03, chapter04, chapter05, chapter06, chapter07, chapter08)
