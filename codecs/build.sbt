
import oncue.build._

OnCue.baseSettings

libraryDependencies ++= Seq(
  "org.typelevel"   %% "scodec-core"          % "1.6.+",
  "oncue.typelevel" %% "shapeless-scalacheck" % "0.4.0" % "test"
)
