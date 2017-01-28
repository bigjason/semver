import sbt._

//noinspection TypeAnnotation
object Dependencies {
  lazy val scalaTest       = "org.scalatest" %% "scalatest" % "3.0.1"
  lazy val fastParse       = "com.lihaoyi" %% "fastparse" % "0.4.2"
  lazy val scalaz          = "org.scalaz" %% "scalaz-core" % "7.2.8"
  lazy val scalazScalatest = "org.typelevel" %% "scalaz-scalatest" % "1.1.1"
  lazy val scalaCheck      = "org.scalacheck" %% "scalacheck" % "1.13.4"
}
