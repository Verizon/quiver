scalaCheckVersion := {
  scalazVersion.value match {
    case VersionNumber(Seq(7, 2, _*), _, _) => "1.12.6"
    case VersionNumber(Seq(7, 1, _*), _, _) =>
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, n)) if n <= 11 => "1.11.4"
        case Some((2, n)) => "1.11.6"
      }
  }
}

resolvers += Resolver.sonatypeRepo("releases")

libraryDependencies += "org.scodec" %% "scodec-core" % "1.10.3"
