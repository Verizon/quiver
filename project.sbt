
organization in Global := "io.verizon.quiver"

scalaVersion in Global := crossScalaVersions.value.head

crossScalaVersions in Global := Seq("2.11.7", "2.10.4")

scalacOptions in Global ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-unchecked",
  "-Xfatal-warnings",
  "-Xlint",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-Xfuture"
)

scalacOptions in (Compile,doc) := Seq("-groups", "-implicits")

lazy val quiver = project.in(file(".")).aggregate(core,codecs,docs)

lazy val core = project

lazy val docs = project.dependsOn(core, codecs)

lazy val codecs = project.dependsOn(core % "test->test;compile->compile")

enablePlugins(DisablePublishingPlugin)

//////////////////// needed for publishing to central ////////////////////

sonatypeProfileName := "io.verizon"

pomExtra in Global := {
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
  </developers>
}

licenses := Seq("Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0.html"))

homepage := Some(url("http://verizon.github.io/quiver/"))

scmInfo := Some(ScmInfo(url("https://github.com/verizon/quiver"),
                            "git@github.com:verizon/quiver.git"))
