import java.util.Scanner

import com.decodified.scalassh._
import net.schmizz.sshj.xfer.FileSystemFile
import sbt.Keys._
import sbt._

import scala.scalajs.sbtplugin.ScalaJSPlugin.ScalaJSKeys._
import scala.scalajs.sbtplugin.ScalaJSPlugin._
import com.lihaoyi.workbench.Plugin._

object GlycanoBuild extends Build {
  val glycanoScalaVersion = "2.11.2"

  lazy val root = project.in(file("."))
    .settings(scalaJSSettings: _*)
    .settings(workbenchSettings: _*)
    .settings(
      name := "GlycanoWeb",
      scalaVersion := glycanoScalaVersion,
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
        Resolver.defaultLocal,
        Resolver.url("sjs-release", url("http://dl.bintray.com/scala-js/scala-js-releases"))(Resolver.ivyStylePatterns),
        "myltsev" at "http://dl.bintray.com/alexander-myltsev/maven"
      ),
      libraryDependencies ++= Seq(
        "org.scala-lang.modules.scalajs"  %%% "scalajs-dom"       % "0.6",
        "org.scala-lang.modules.scalajs"  %%% "scalajs-jquery"    % "0.6",
        "com.scalatags"                   %%% "scalatags"         % "0.4.2",
        "com.scalarx"                     %%% "scalarx"           % "0.2.6",
        "com.github.japgolly.fork.scalaz" %%% "scalaz-core"       % "7.1.0-4",
        "name.myltsev"                    %%% "shapeless"         % "2.0.0",
        "org.parboiled"                   %%% "parboiled"         % "2.0.1_2",
        "com.lihaoyi"                     %%% "upickle"           % "0.2.5"
      ),
      bootSnippet := "za.jwatson.glycanoweb.GlycanoWeb().main();",
      refreshBrowsers <<= refreshBrowsers.triggeredBy(fastOptJS in Compile),
      relativeSourceMaps := true
    )
}