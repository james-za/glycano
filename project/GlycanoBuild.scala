
import sbt.Keys._
import sbt._

import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._

import com.lihaoyi.workbench.Plugin._

object GlycanoBuildSettings {
  val paradiseVersion = "2.1.0-M5"
  val scalajsReactVersion = "0.8.4"
  val monocleVersion = "1.1.1"

  lazy val buildSettings = Seq(
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
    addCompilerPlugin("org.scalamacros" % "paradise" % paradiseVersion cross CrossVersion.full)
  )
}

object GlycanoBuild extends sbt.Build {
  import GlycanoBuildSettings._

  lazy val glycanoweb = project.in(file("core"))
    .enablePlugins(ScalaJSPlugin)
    .settings(workbenchSettings: _*)
    .settings(buildSettings: _*)
    .settings(
      libraryDependencies ++= Seq(
        "org.scala-js"                      %%% "scalajs-dom"    % "0.8.0",
        "be.doeraene"                       %%% "scalajs-jquery" % "0.8.0",
        //"com.lihaoyi"                       %%% "scalatags"      % "0.4.5",
        "com.lihaoyi"                       %%% "scalarx"        % "0.2.7",
        "com.lihaoyi"                       %%% "upickle"        % "0.2.6",
        "com.github.japgolly.fork.scalaz"   %%% "scalaz-core"    % "7.1.1",
        "com.github.japgolly.scalajs-react" %%% "core"           % scalajsReactVersion,
        "com.github.japgolly.scalajs-react" %%% "extra"          % scalajsReactVersion,
        "com.github.japgolly.scalajs-react" %%% "ext-monocle"    % scalajsReactVersion,
        "com.github.japgolly.scalajs-react" %%% "ext-scalaz71"   % scalajsReactVersion,
        "com.github.japgolly.fork.monocle"  %%% "monocle-core"   % monocleVersion,
        "com.github.japgolly.fork.monocle"  %%% "monocle-macro"  % monocleVersion,
        "name.myltsev"                      %%% "shapeless"      % "2.0.0",
        "org.parboiled"                     %%% "parboiled"      % "2.0.1"
      ))
    .settings(
      name := "GlycanoWeb",
      bootSnippet := "za.jwatson.glycanoweb.GlycanoWeb().main();",
      refreshBrowsers <<= refreshBrowsers.triggeredBy(fastOptJS in Compile),
      relativeSourceMaps := true
    )
}