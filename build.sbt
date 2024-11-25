ThisBuild / version      := "1.0.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.5.2"
ThisBuild / organization := "eu.derfniw"

lazy val root = (project in file("."))
  .settings(name := "AoC2024")
  .aggregate(util.project +: solutions.refs: _*)

lazy val util = (project in file("util"))
  .settings(commonSettings)

lazy val solutions = new CompositeProject {
  override def componentProjects = {
    val dayDirs = file("solutions/")
      .listFiles(f => f.isDirectory && f.getName.startsWith("day"))

    dayDirs.map { dir =>
      Project(dir.getName, dir)
        .settings(commonSettings)
        .dependsOn(util)
    }.toSeq
  }

  def refs = componentProjects.map(_.project)
}

lazy val commonSettings = Seq(
  libraryDependencies += "org.scalameta" %% "munit" % "1.0.2" % Test
)
