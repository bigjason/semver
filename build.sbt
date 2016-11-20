import Dependencies._

crossScalaVersions in ThisBuild := Seq("2.11.8", "2.12.0")
scalaVersion in ThisBuild := crossScalaVersions.value.head

scalacOptions in ThisBuild ++= Seq("-deprecation"
                                   , "-encoding", "UTF-8"
                                   , "-feature"
                                   , "-unchecked"
                                   , "-Xlint"
                                   , "-Ywarn-unused-import")

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.bigjason"
    )),
    name := "semver",
    version := "0.1.0",
    coverageHighlighting := true,
    libraryDependencies ++= Seq(
      scalaTest % Test,
      fastParse
    )
  )

addCommandAlias("testCoverage", ";coverage; clean; test; coverageReport")