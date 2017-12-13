val CatsVersion = "1.0.0-RC1"

libraryDependencies ++= Seq(
  "org.typelevel"              %% "cats-free"                 % CatsVersion,
  "org.typelevel"              %% "cats-laws"                 % CatsVersion % "test",
  "com.github.alexarchambault" %% "scalacheck-shapeless_1.13" % "1.1.7"     % "test"
)

scalacOptions in (Compile,doc) := Seq("-groups", "-implicits")
