// scalastyle:off


// *** Settings ***

useGpg := false

lazy val commonSettings = Seq(
  organization := "nl.rabobank.oss.rules",
  organizationHomepage := Some(url("https://github.com/rabobank-nederland")),
  homepage := Some(url("https://github.com/rabobank-nederland/rule-viewer")),
  version := "0.1.1-SNAPSHOT",
  scalaVersion := "2.11.8",
  scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-Xlint", "-Xfatal-warnings")
) ++ staticAnalysisSettings ++ publishSettings


// *** Projects ***

lazy val ruleViewer = (project in file("."))
  .settings(commonSettings: _*)
  .settings(
    name := "rule-viewer",
    description := "Rabo Rules Rule Viewer",
    libraryDependencies ++= dependencies
  )
  .enablePlugins(PlayScala, UniversalDeployPlugin)


// *** Dependencies ***

lazy val raboRulesVersion = "0.6.0"
lazy val scalaTestVersion = "2.2.5"
lazy val jodaTimeVersion = "2.4"
lazy val jodaConvertVersion = "1.8"

lazy val dependencies = Seq(
  "nl.rabobank.oss.rules" %% "rule-engine" % raboRulesVersion,
  "nl.rabobank.oss.rules" %% "rule-rest" % "0.1.0",
  "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.7.2",
  "com.fasterxml.jackson.jaxrs" % "jackson-jaxrs-json-provider" % "2.7.3",
  "joda-time" % "joda-time" % jodaTimeVersion,
  "org.joda" % "joda-convert" % jodaConvertVersion,
  "org.scalatest" %% "scalatest" % scalaTestVersion % Test,
  "org.scalacheck" %% "scalacheck" % "1.12.5" % Test,
  "com.storm-enroute" %% "scalameter" % "0.7" % Test
)

// *** Static analysis ***

lazy val staticAnalysisSettings = {
  lazy val compileScalastyle = taskKey[Unit]("Runs Scalastyle on production code")
  lazy val testScalastyle = taskKey[Unit]("Runs Scalastyle on test code")

  Seq(
    scalastyleConfig in Compile := (baseDirectory in ThisBuild).value / "project" / "scalastyle-config.xml",
    scalastyleConfig in Test := (baseDirectory in ThisBuild).value / "project" / "scalastyle-test-config.xml",

    // The line below is needed until this issue is fixed: https://github.com/scalastyle/scalastyle-sbt-plugin/issues/44
    scalastyleConfig in scalastyle := (baseDirectory in ThisBuild).value / "project" / "scalastyle-test-config.xml",

    compileScalastyle := org.scalastyle.sbt.ScalastylePlugin.scalastyle.in(Compile).toTask("").value,
    testScalastyle := org.scalastyle.sbt.ScalastylePlugin.scalastyle.in(Test).toTask("").value
  )
}

addCommandAlias("verify", ";compileScalastyle;testScalastyle;coverage;test;coverageReport;coverageAggregate")
addCommandAlias("release", ";clean;compile;publishLocal;publishSigned")

// *** Publishing ***

lazy val publishSettings = Seq(
  pomExtra := pom,
  publishMavenStyle := true,
  pomIncludeRepository := { _ => false },
  licenses := Seq("MIT License" -> url("http://www.opensource.org/licenses/mit-license.php")),
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases"  at nexus + "service/local/staging/deploy/maven2")
  }
)

val packageDist = taskKey[File]("package-dist")

packageDist := (baseDirectory in Compile).value / "target" / "universal" / (name.value + "-" + version.value + ".tgz")

artifact in (Universal, packageDist) ~= { (art:Artifact) => art.copy(`type` = "tgz", extension = "tgz") }

addArtifact(artifact in (Universal, packageDist), packageDist in Universal)

publish <<= (publish) dependsOn (packageZipTarball in Universal)
publishM2 <<= (publishM2) dependsOn (packageZipTarball in Universal)
publishLocal <<= (publishLocal) dependsOn (packageZipTarball in Universal)

lazy val pom =
  <developers>
    <developer>
      <name>Vincent Zorge</name>
      <email>scala-rules@linuse.nl</email>
      <organization>Linuse</organization>
      <organizationUrl>https://github.com/vzorge</organizationUrl>
    </developer>
    <developer>
      <name>Jan-Hendrik Kuperus</name>
      <email>jan-hendrik@scala-rules.org</email>
      <organization>Yoink Development</organization>
      <organizationUrl>http://www.yoink.nl</organizationUrl>
    </developer>
    <developer>
      <name>Nathan Perdijk</name>
      <email>nathan@scala-rules.org</email>
    </developer>
  </developers>
  <scm>
    <connection>scm:git:git@github.com:rabobank-nederland/rule-viewer.git</connection>
    <developerConnection>scm:git:git@github.com:rabobank-nederland/rule-viewer.git</developerConnection>
    <url>git@github.com:rabobank-nederland/rule-viewer.git</url>
  </scm>
  
