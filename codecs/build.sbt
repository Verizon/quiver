
common.testSettings

common.publishSettings

resolvers += Resolver.sonatypeRepo("releases")

libraryDependencies ++= Seq(
  "org.scodec" %% "scodec-core" % "1.8.1",
  "org.typelevel" %% "shapeless-scalacheck" % "0.4" % "test"
)
