resolvers += Resolver.url("artifactory", url("http://scalasbt.artifactoryonline.com/scalasbt/sbt-plugin-releases"))(Resolver.ivyStylePatterns)

resolvers += Resolver.url("sbt-plugin-snapshots", url("http://repo.scala-sbt.org/scalasbt/sbt-plugin-snapshots/"))(Resolver.ivyStylePatterns)

addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "2.1.1")

addSbtPlugin("com.typesafe.sbt" % "sbt-pgp" % "0.8")

resolvers += "Sonatype OSS Releases" at "https://oss.sonatype.org/content/repositories/snapshots"

resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/service/local/staging/deploy/maven2"

