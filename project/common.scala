import sbt._, Keys._
import sbtrelease._
import sbtrelease.ReleasePlugin.autoImport._
import sbtrelease.ReleaseStateTransformations._
import sbtrelease.Utilities._
import bintray.BintrayKeys._

object common {

  def settings =
    bintraySettings ++
    releaseSettings ++
    publishingSettings ++
    testSettings

  val scalaTestVersion  = SettingKey[String]("scalatest version")
  val scalaCheckVersion = SettingKey[String]("scalacheck version")

  def testSettings = Seq(
    scalaTestVersion     := "2.2.5",
    scalaCheckVersion    := "1.12.3",
    libraryDependencies ++= Seq(
      "org.scalatest"  %% "scalatest"  % scalaTestVersion.value  % "test",
      "org.scalacheck" %% "scalacheck" % scalaCheckVersion.value % "test"
    )
  )

  def ignore = Seq(
    publish := (),
    publishLocal := (),
    publishArtifact in Test := false,
    publishArtifact in Compile := false
  )

  def bintraySettings = Seq(
    bintrayPackageLabels := Seq("graph", "functional programming", "scala", "reasonable"),
    bintrayOrganization := Some("oncue"),
    bintrayRepository := "releases",
    bintrayPackage := "quiver"
  )

  def releaseSettings = Seq(
    releaseCrossBuild := false,
    releaseVersion := { ver =>
      sys.env.get("TRAVIS_BUILD_NUMBER").orElse(sys.env.get("BUILD_NUMBER"))
        .map(s => try Option(s.toInt) catch { case _: NumberFormatException => Option.empty[Int] })
        .flatMap(ci => Version(ver).map(_.withoutQualifier.copy(bugfix = ci).string))
        .orElse(Version(ver).map(_.withoutQualifier.string))
        .getOrElse(versionFormatError)
    },
    releaseTagName :=
      s"${scalaVersion.value.take(4)}/v${if (releaseUseGlobalVersion.value) (version in ThisBuild).value else version.value}",
    releaseProcess := Seq[ReleaseStep](
      checkSnapshotDependencies,
      inquireVersions,
      setReleaseVersion,
      tagRelease,
      runTest,
      publishArtifacts,
      pushChanges.copy(check = identity)
    )
  )

  def publishingSettings = Seq(
    pomExtra := (
      <developers>
        <developer>
          <id>timperrett</id>
          <name>Timothy Perrett</name>
          <url>http://github.com/timperrett</url>
        </developer>
        <developer>
          <id>runarorama</id>
          <name>Runar Bjarnason</name>
          <url>http://github.com/runarorama</url>
        </developer>
        <developer>
          <id>stew</id>
          <name>Stew O'Connor</name>
          <url>http://github.com/stew</url>
        </developer>
      </developers>),
    publishMavenStyle := true,
    licenses := Seq("Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0.html")),
    homepage := Some(url("http://oncue.github.io/quiver/")),
    scmInfo := Some(ScmInfo(url("https://github.com/oncue/quiver"),
                                "git@github.com:oncue/quiver.git")),
    pomIncludeRepository := { _ => false },
    publishArtifact in Test := false
  )
}
