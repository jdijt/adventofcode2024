ThisBuild / version      := "1.0.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.5.2"
ThisBuild / organization := "eu.derfniw"

lazy val root = (project in file("."))
  .settings(name := "AoC2024")
  .settings(
    libraryDependencies += "org.scalameta" %% "munit" % "1.0.2" % Test
  )

