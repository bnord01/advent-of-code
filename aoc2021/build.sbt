ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.7"

ThisBuild / scalacOptions += "-deprecation"

ThisBuild / scalacOptions += "-unchecked"

lazy val root = (project in file("."))
  .settings(
    name := "advent_of_code_2021"
  )
