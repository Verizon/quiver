
libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.2.8",
  "org.scalaz" %% "scalaz-scalacheck-binding" % "7.2.8" % "test")

scalacOptions in (Compile,doc) := Seq("-groups", "-implicits")
