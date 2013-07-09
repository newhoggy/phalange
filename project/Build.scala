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
    .aggregate(phalange).settings(commonSettings: _*)
    .aggregate(aa).settings(commonSettings: _*)
    .aggregate(ab).settings(commonSettings: _*)

  lazy val phalange = Project(id = "phalange", base = file("phalange")).settings(commonSettings: _*)

  lazy val aa = Project(id = "aa", base = file("aa")).settings(commonSettings: _*)

  lazy val ab = Project(id = "ab", base = file("ab")).settings(commonSettings: _*)
}


