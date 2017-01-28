import Dependencies._

crossScalaVersions in ThisBuild := Seq("2.11.8", "2.12.1")
scalaVersion in ThisBuild := crossScalaVersions.value.head

scalacOptions in ThisBuild ++= Seq("-deprecation"
                                   , "-encoding", "UTF-8"
                                   , "-feature"
                                   , "-unchecked"
                                   , "-Xlint"
                                   , "-Ywarn-unused-import")

scalacOptions in(Compile, doc) ++= Seq("-skip-packages", "com.bigjason.semver.internal")

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.bigjason"
    )),
    name := "semver",
    version := "0.2.0",
    coverageHighlighting := true,
    libraryDependencies ++= Seq(fastParse
                                , scalaTest % Test
                                , scalaCheck % Test)
  )

addCommandAlias("testCoverage", ";coverage; clean; test; coverageReport")