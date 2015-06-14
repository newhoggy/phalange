import sbt._
import Keys._

object JohnsFingerTree extends Build {
  lazy val buildSettings = Seq(
      organization := "com.timesprint",
      scalaVersion := "2.10.1",
      scalacOptions := Seq("-feature", "-deprecation", "-unchecked", "-Xlint", "-Yrangepos", "-encoding", "utf8"),
      scalacOptions in (console) += "-Yrangepos"
  )

  lazy val commonSettings = Defaults.defaultSettings ++ buildSettings

  lazy val root = Project(id = "livefx", base = file("."))
    .aggregate(cj).settings(commonSettings: _*)

  lazy val cj = Project(id = "cj", base = file("cj")).settings(commonSettings: _*)
}

