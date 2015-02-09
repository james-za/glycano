
import sbt.Keys._
import sbt._

import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._

import com.lihaoyi.workbench.Plugin._

object GlycanoBuild extends sbt.Build {
  lazy val root = project.in(file("."))
    .enablePlugins(ScalaJSPlugin)
    .settings(workbenchSettings: _*)
    .settings(
      name := "GlycanoWeb",
      scalaVersion := "2.11.5",
      scalacOptions ++= Seq(
        "-deprecation",
        "-unchecked",
        "-feature",
        "-encoding", "utf8"
      ),
      version := "0.1-SNAPSHOT",
      resolvers ++= Seq(
        Resolver.sonatypeRepo("snapshots"),
        Resolver.sonatypeRepo("releases"),
        Resolver.url("sjs-release", url("http://dl.bintray.com/scala-js/scala-js-releases"))(Resolver.ivyStylePatterns),
        "myltsev" at "http://dl.bintray.com/alexander-myltsev/maven"
      ),
      libraryDependencies ++= Seq(
        "org.scala-js"                      %%% "scalajs-dom"       % "0.7.0",
        "be.doeraene"                       %%% "scalajs-jquery"    % "0.7.0",
        "com.lihaoyi"                       %%% "scalatags"         % "0.4.5",
        "com.lihaoyi"                       %%% "scalarx"           % "0.2.7",
        "com.lihaoyi"                       %%% "upickle"           % "0.2.6",
        "com.github.japgolly.fork.scalaz"   %%% "scalaz-core"       % "7.1.1",
        "com.github.japgolly.scalajs-react" %%% "core"              % "0.7.2",
        "com.github.japgolly.scalajs-react" %%% "ext-monocle"       % "0.7.2",
        "com.github.japgolly.scalajs-react" %%% "ext-scalaz71"      % "0.7.2",
        "com.github.japgolly.fork.monocle"  %%% "monocle-core"      % "1.0.1",
        "com.github.japgolly.fork.monocle"  %%% "monocle-macro"     % "1.0.1",
        "name.myltsev"                      %%% "shapeless"         % "2.0.0",
        "org.parboiled"                     %%% "parboiled"         % "2.0.1"
      ),
      bootSnippet := "za.jwatson.glycanoweb.GlycanoWeb().main();",
      refreshBrowsers <<= refreshBrowsers.triggeredBy(fastOptJS in Compile),
      relativeSourceMaps := true,
      addCompilerPlugin(compilerPlugin("org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.full))
    )
}