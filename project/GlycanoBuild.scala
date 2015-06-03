import sbt.Keys._
import sbt._

import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._

import com.lihaoyi.workbench.Plugin._

object GlycanoBuildSettings {
  val paradiseVersion = "2.1.0-M5"
  val scalajsReactVersion = "0.9.0"
  val monocleVersion = "1.1.1"

  def jsMinified(name: String) = ProvidedJS / s"js/$name.js" minified s"js/$name.min.js"
}

object GlycanoBuild extends sbt.Build {
  import GlycanoBuildSettings._

  lazy val glycanoweb = project.in(file(".")).aggregate(core)

  lazy val core = project.in(file("core"))
    .enablePlugins(ScalaJSPlugin)
    .settings(workbenchSettings: _*)
    .settings(buildSettings: _*)
    .settings(
      incOptions := incOptions.value.withNameHashing(nameHashing = false),
      scalaVersion := "2.11.6",
      scalacOptions ++= Seq(
        "-deprecation",
        "-unchecked",
        "-feature",
        "-encoding", "utf8",
        "-language:higherKinds"
      ),
      version := "0.1-SNAPSHOT",
      resolvers ++= Seq(
        Resolver.sonatypeRepo("snapshots"),
        Resolver.sonatypeRepo("releases"),
        Resolver.url("sjs-release", url("http://dl.bintray.com/scala-js/scala-js-releases"))(Resolver.ivyStylePatterns),
        "myltsev" at "http://dl.bintray.com/alexander-myltsev/maven"
      ),
      addCompilerPlugin("org.scalamacros" % "paradise" % paradiseVersion cross CrossVersion.full),
      libraryDependencies ++= Seq(
        "org.scala-js"                      %%% "scalajs-dom"    % "0.8.0",
        "be.doeraene"                       %%% "scalajs-jquery" % "0.8.0",
        //"com.lihaoyi"                       %%% "scalatags"      % "0.4.5",
        "com.lihaoyi"                       %%% "upickle"        % "0.2.6",
        "com.github.japgolly.fork.scalaz"   %%% "scalaz-core"    % "7.1.1-2",
        "com.github.japgolly.scalajs-react" %%% "core"           % scalajsReactVersion,
        "com.github.japgolly.scalajs-react" %%% "extra"          % scalajsReactVersion,
        "com.github.japgolly.fork.monocle"  %%% "monocle-core"   % monocleVersion,
        "com.github.japgolly.fork.monocle"  %%% "monocle-macro"  % monocleVersion,
        "name.myltsev"                      %%% "shapeless"      % "2.0.0",
        "org.parboiled"                     %%% "parboiled"      % "2.0.1"
      ),
//      jsDependencies ++= Seq(
//        ProvidedJS / "js/filereader.js",
//        ProvidedJS / "js/saveSvgAsPng.js",
//        "org.webjars" % "react" % "0.12.2" / "react-with-addons.js" minified "react-with-addons.min.js"
//      ),
      name := "GlycanoWeb",
      bootSnippet := "za.jwatson.glycanoweb.GlycanoWeb().main();",
      refreshBrowsers <<= refreshBrowsers.triggeredBy(fastOptJS in Compile),
      relativeSourceMaps := true
//      skip in packageJSDependencies := false,
//      skip in packageMinifiedJSDependencies := false
    )
}