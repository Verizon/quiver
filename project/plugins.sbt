// Comment to get more information during initialization
logLevel := Level.Warn

resolvers ++= Seq(
  "Sonatype Nexus Central Repository" at "http://nexus.svc.m.infra-host.com/nexus/content/repositories/central/",
  "Sonatype Nexus Typesafe Repository" at "http://nexus.svc.m.infra-host.com/nexus/content/repositories/typesafe/",
  "Sonatype Nexus Sonatype Repository" at "http://nexus.svc.m.infra-host.com/nexus/content/repositories/sonatype/",
  "Sonatype Nexus Artifactoryonline Repository" at "http://nexus.svc.m.infra-host.com/nexus/content/repositories/artifactoryonline/",
  "Sonatype Nexus Releases Repository" at "http://nexus.svc.m.infra-host.com/nexus/content/repositories/releases/",
  "Sonatype Nexus 3rd party repository" at "http://nexus.svc.m.infra-host.com/nexus/content/repositories/thirdparty/",
  "Sonatype Nexus Spray repository" at "http://nexus.svc.m.infra-host.com/nexus/content/repositories/spray/",
  Resolver.url("Sonatype Nexus Artifactoryonline Scalasbt Repository", url("http://nexus.svc.m.infra-host.com/nexus/content/repositories/artifactoryonline-scalasbt/"))(Resolver.ivyStylePatterns)
)

libraryDependencies ++= Seq(
)

addSbtPlugin("com.intel.media" %% "mediabuild" % "1.1.+")

addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "2.1.0")

credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")
