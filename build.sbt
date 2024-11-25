ThisBuild / version := "1.0.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.5.2"
ThisBuild / organization := "eu.derfniw"

lazy val root = (project in file("."))
  .aggregate(util)
  .aggregate(dayProjects)

lazy val util = project in file("util")

lazy val dayProjects = (1 to 25).map(n => {
  val padded = n.toString.reverse.padTo(2, '0').reverse
  Project("day" + padded, file("day" + padded))
})
