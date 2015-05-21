
organization in Global  := "oncue.quiver"

scalaVersion in Global  := "2.10.5"

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

lazy val quiver = project.in(file(".")).aggregate(core,codecs)

lazy val core = project

lazy val codecs = project.dependsOn(core)

publish := {}
