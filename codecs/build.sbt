
common.testSettings

common.publishSettings

resolvers ++= Seq(
  "scalaz.bintray" at "http://dl.bintray.com/scalaz/releases",
  "oncue.bintray" at "http://dl.bintray.com/oncue/releases"
)

libraryDependencies ++= Seq(
  "org.typelevel"   %% "scodec-core"          % "1.6.+",
  "oncue.typelevel" %% "shapeless-scalacheck" % "0.4.0" % "test"
)
