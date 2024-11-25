ThisBuild / version := "1.0.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.5.2"
ThisBuild / organization := "eu.derfniw"

lazy val root = (project in file("."))
  .settings(name := "AoC2024")
  .aggregate(util.project +: dayProjects.refs: _*)

lazy val util = project in file("util")

lazy val dayProjects = new CompositeProject {
  override def componentProjects = {
    val dayDirs = file(".")
      .listFiles(f => f.isDirectory && f.getName.startsWith("day"))

    dayDirs.map(dir => {
      Project(dir.getName, dir)
        .dependsOn(util)
    }).toSeq
  }

  def refs = componentProjects.map(_.project)
}
