import sbt._
import Keys._

object ProjectBuild extends Build {
  lazy val project = "phalange"

  lazy val root = Project(
    id = project,
    base = file("."),
    settings = Project.defaultSettings
  ).settings(
    organization := "",
    version := "0.1-SNAPSHOT",
    scalaVersion := "2.10.1",

    resolvers ++= Seq(
      "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
      "Typesafe Snapshots Repository" at "http://repo.typesafe.com/typesafe/snapshots/",
      "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/",
      "releases" at "http://oss.sonatype.org/content/repositories/releases"
    ),

    libraryDependencies ++= Seq(
      "com.typesafe" %% "scalalogging-slf4j" % "1.0.1",
      "junit" % "junit" % "4.10" % "test",
      "org.scalaz" %% "scalaz-core" % "7.0.0",
      "org.specs2" %% "specs2" % "1.14" % "test"
    ),

    scalacOptions ++= Seq("-deprecation", "-unchecked")
  )
}

