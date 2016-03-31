import com.typesafe.sbt.SbtSite.SiteKeys._
import com.typesafe.sbt.SbtGhPages.GhPagesKeys._
import sbtunidoc.Plugin.UnidocKeys._

name := "docs"

site.settings

tutSettings

site.addMappingsToSiteDir(tut, "tut")

unidocSettings

site.addMappingsToSiteDir(mappings in (ScalaUnidoc, packageDoc), "api")

ghpages.settings

ghpagesNoJekyll := false

includeFilter in makeSite := "*.yml" | "*.md" | "*.html" | "*.css" | "*.png" | "*.jpg" | "*.gif" | "*.js" | "*.swf"

git.remoteRepo := "git@github.com:oncue/quiver.git"

scalacOptions in (ScalaUnidoc, sbtunidoc.Plugin.UnidocKeys.unidoc) ++= Seq(
  "-doc-source-url", "https://github.com/oncue/quiver/blob/master€{FILE_PATH}.scala",
  "-sourcepath", baseDirectory.in(LocalRootProject).value.getAbsolutePath,
  "-groups",
  "-implicits",
  "-skip-packages", "scalaz"
)

publish := ()

publishLocal := ()
