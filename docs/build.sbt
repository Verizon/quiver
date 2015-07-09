import com.typesafe.sbt.SbtSite.SiteKeys._
import com.typesafe.sbt.SbtGhPages.GhPagesKeys._

publish := ()

publishLocal := ()

publishArtifact in (Compile, packageBin) := false

site.settings

tutSettings

site.addMappingsToSiteDir(tut, "tut")

ghpages.settings

ghpagesNoJekyll := false

includeFilter in makeSite := "*.html" | "*.css" | "*.png" | "*.jpg" | "*.gif" | "*.js" | "*.swf" | "*.yml" | "*.md"

git.remoteRepo := "git@github.oncue.verizon.net:arch/quiver.git"
