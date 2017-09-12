
organization in Global := "io.verizon.quiver"

scalaVersion in Global := crossScalaVersions.value.head

crossScalaVersions in Global := Seq("2.12.3", "2.11.11", "2.10.6")

scalacOptions in (Compile,doc) := Seq("-groups", "-implicits")

lazy val quiver = project.in(file(".")).aggregate(core,codecs,docs)

lazy val core = project

lazy val docs = project.dependsOn(core, codecs)

lazy val codecs = project.dependsOn(core % "test->test;compile->compile")

enablePlugins(DisablePublishingPlugin)
