ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.7"

ThisBuild / scalacOptions += "-deprecation"

ThisBuild / scalacOptions += "-unchecked"

libraryDependencies += "org.parboiled" %% "parboiled" % "2.3.0"

libraryDependencies += "org.scalanlp" %% "breeze" % "2.0.1-RC1"

lazy val root = (project in file("."))
  .settings(
    name := "advent_of_code_2021"
  )
