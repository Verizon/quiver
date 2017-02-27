//: ----------------------------------------------------------------------------
//: Copyright (C) 2017 Verizon.  All Rights Reserved.
//:
//:   Licensed under the Apache License, Version 2.0 (the "License");
//:   you may not use this file except in compliance with the License.
//:   You may obtain a copy of the License at
//:
//:       http://www.apache.org/licenses/LICENSE-2.0
//:
//:   Unless required by applicable law or agreed to in writing, software
//:   distributed under the License is distributed on an "AS IS" BASIS,
//:   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//:   See the License for the specific language governing permissions and
//:   limitations under the License.
//:
//: ----------------------------------------------------------------------------
package verizon.build

import sbt._, Keys._

object ScalazPlugin extends AutoPlugin {
  object autoImport {
    val scalazVersion = settingKey[String]("scalaz version")
    val scalazCrossVersioner = settingKey[String => String]("modifies a version number according to scalaz version")
  }

  import autoImport._

  override def requires = RigPlugin

  override def trigger = allRequirements

  override lazy val projectSettings = Seq(
    scalazVersion := sys.env.get("SCALAZ_VERSION").getOrElse("7.2.7"),
    scalazCrossVersioner := scalazCrossVersioners.default(scalazVersion.value),
    version := scalazCrossVersioner.value(version.value),
    unmanagedSourceDirectories in Compile += (sourceDirectory in Compile).value / VersionNumber(scalazVersion.value).numbers.take(2).mkString("scalaz-", ".", "")
  )

  object scalazCrossVersioners {
    /**
     * Appends `-scalaz-X.Y` to the version number as a qualifier.  For
     * example, version 1.0.0 applied to scalaz-7.2.7 becomes `1.0.0-scalaz-7.2`
     */
    def default(scalazVersion: String): String => String =
      _ match {
        case VersionNumber(numbers, tags, extras) =>
          val qualifier = scalazVersion match {
            case VersionNumber(Seq(7, zMinor, _*), Seq(), _) =>
              s"scalaz-7.$zMinor"
            case _ =>
              //
              s"scalaz-$scalazVersion"
          }
          numbers.mkString(".") + (qualifier +: tags).mkString("-", "-", "") + extras.mkString("")
      }

    def suffixed(scalazVersion: String)(suffixer: VersionNumber => String): String => String =
      _ match {
        case VersionNumber(numbers, tags, extras) =>
          val suffix = suffixer(VersionNumber(scalazVersion))
          numbers.mkString(".") + suffix + (tags match {
            case Seq() => ""
            case ts => ts.mkString("-", "-", "")
          }) + extras.mkString("")
      }

    /**
     * This convention was started with scalaz-stream-0.8, and
     * followed by certain versions of http4s, doobie, and argonaut.
     * It is not recommended, as it breaks semantic versioning and
     * `sbt.VersionNumber` parsing.
     */
    def `scalaz-stream-0.8`(scalazVersion: String): String => String =
      suffixed(scalazVersion) {
        case VersionNumber(Seq(7, 2, _*), _, _) => "a"
        case _ => ""
      }
  }
}
