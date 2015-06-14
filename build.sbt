scalaVersion in ThisBuild := "2.10.1"

publishMavenStyle := true

publishTo <<= version { (v: String) =>
  val nexus = "https://oss.sonatype.org/"
  if (v.trim.endsWith("SNAPSHOT"))
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

publishArtifact in Test := false

pomExtra := (
  <url>http://timesprint.com/livefx</url>
  <licenses>
    <license>
      <name>BSD-style</name>
      <url>http://www.opensource.org/licenses/bsd-license.php</url>
      <distribution>repo</distribution>
    </license>
  </licenses>
  <scm>
    <url>git@github.com:newhoggy/livefx.git</url>
    <connection>scm:git:git@github.com:newhoggy/livefx.git</connection>
  </scm>
  <developers>
    <developer>
      <id>newhoggy</id>
      <name>John Ky</name>
      <url>http://timesprint.com</url>
    </developer>
  </developers>)
