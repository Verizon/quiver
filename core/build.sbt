import com.typesafe.sbt.SbtSite.SiteKeys._
import com.typesafe.sbt.SbtGhPages.GhPagesKeys._

OnCue.baseSettings

libraryDependencies ++= Seq(
  "org.scalaz"     %% "scalaz-core" % "7.1.2",
  "org.scalacheck" %% "scalacheck"  % "1.12.2" % "test"
)
