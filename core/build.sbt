
common.settings

resolvers += "scalaz.bintray" at "http://dl.bintray.com/scalaz/releases"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.2"

scalacOptions in (Compile,doc) := Seq("-groups", "-implicits")
