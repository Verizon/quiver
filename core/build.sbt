
import oncue.build._

OnCue.baseSettings

libraryDependencies ++= Seq(
  "org.scalaz"     %% "scalaz-core" % "7.1.2",
  "org.scalacheck" %% "scalacheck"  % "1.12.2" % "test"
)
