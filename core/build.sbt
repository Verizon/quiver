
libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.1.2",
  "org.scalaz" %% "scalaz-scalacheck-binding" % "7.1.2" % "test")

scalacOptions in (Compile,doc) := Seq("-groups", "-implicits")
