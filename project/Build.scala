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
    .aggregate(ba).settings(commonSettings: _*)
    .aggregate(ca).settings(commonSettings: _*)
    .aggregate(cb).settings(commonSettings: _*)
    .aggregate(cc).settings(commonSettings: _*)
    .aggregate(cd).settings(commonSettings: _*)
    .aggregate(ce).settings(commonSettings: _*)
    .aggregate(cf).settings(commonSettings: _*)
    .aggregate(cg).settings(commonSettings: _*)
    .aggregate(ch).settings(commonSettings: _*)
    .aggregate(ci).settings(commonSettings: _*)

  lazy val phalange = Project(id = "phalange", base = file("phalange")).settings(commonSettings: _*)

  lazy val aa = Project(id = "aa", base = file("aa")).settings(commonSettings: _*)

  lazy val ab = Project(id = "ab", base = file("ab")).settings(commonSettings: _*)

  lazy val ba = Project(id = "ba", base = file("ba")).settings(commonSettings: _*)

  lazy val ca = Project(id = "ca", base = file("ca")).settings(commonSettings: _*)

  lazy val cb = Project(id = "cb", base = file("cb")).settings(commonSettings: _*)

  lazy val cc = Project(id = "cc", base = file("cc")).settings(commonSettings: _*)

  lazy val cd = Project(id = "cd", base = file("cd")).settings(commonSettings: _*)

  lazy val ce = Project(id = "ce", base = file("ce")).settings(commonSettings: _*)

  lazy val cf = Project(id = "cf", base = file("cf")).settings(commonSettings: _*)

  lazy val cg = Project(id = "cg", base = file("cg")).settings(commonSettings: _*)

  lazy val ch = Project(id = "ch", base = file("ch")).settings(commonSettings: _*)

  lazy val ci = Project(id = "ci", base = file("ci")).settings(commonSettings: _*)
}

