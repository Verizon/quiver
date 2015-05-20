resolvers += Resolver.url(
  "tpolecat-sbt-plugin-releases",
    url("http://dl.bintray.com/content/tpolecat/sbt-plugin-releases"))(
        Resolver.ivyStylePatterns)

resolvers += "im.nexus" at "http://nexus.svc.oncue.com/nexus/content/groups/intel_media_maven/"

credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")

addSbtPlugin("com.eed3si9n"     % "sbt-unidoc"  % "0.3.2")

addSbtPlugin("com.typesafe.sbt" % "sbt-ghpages" % "0.5.3")

addSbtPlugin("com.typesafe.sbt" % "sbt-site"    % "0.8.1")

addSbtPlugin("org.tpolecat"     % "tut-plugin"  % "0.3.2")

addSbtPlugin("oncue.build" %% "sbt-oncue" % "7.0.+")
