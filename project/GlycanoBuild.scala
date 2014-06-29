import java.util.Scanner

import com.decodified.scalassh._
import net.schmizz.sshj.xfer.FileSystemFile
import sbt.Keys._
import sbt._

import scala.scalajs.sbtplugin.ScalaJSPlugin.ScalaJSKeys._
import scala.scalajs.sbtplugin.ScalaJSPlugin._

object GlycanoBuild extends Build {
  val glycanoScalaVersion = "2.11.1"
  val paradiseVersion = "2.0.0"

  val gwDefaultSettings: Seq[Setting[_]] = Defaults.defaultSettings ++ Seq(
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
      Resolver.url(
        "bintray-scala-js-releases",
        url("http://dl.bintray.com/scala-js/scala-js-releases")
      )(Resolver.ivyStylePatterns)
    ),
    javaOptions += "-Xmx6G",
    libraryDependencies ++= Seq(
      "org.scala-lang.modules.scalajs" %%% "scalajs-dom"       % "0.6",
      "org.scala-lang.modules.scalajs" %%% "scalajs-jquery"    % "0.6",
      "com.scalatags"                  %%% "scalatags"         % "0.3.8",
      "com.scalarx"                    %%% "scalarx"           % "0.2.5",
      "org.scalaz"                     %%% "scalaz-core"       % "7.0.6",
      "com.chuusai"                    %%% "shapeless"         % "2.0.0",
      "org.typelevel"                  %%% "shapeless-scalaz"  % "0.3-SNAPSHOT"
    )
  )

  lazy val root = project.in(file(".")).settings(
    gwDefaultSettings ++ scalaJSSettings ++ Seq(
      name := "GlycanoWeb",
      nightmareTask
    ): _*
  )

  lazy val nightmare = taskKey[Unit]("Upload fullOptJS output to nightmare")
  lazy val nightmareTask = nightmare := {
    val js = (fullOptJS in Compile).value
    val console = System.console()
    console.writer().print("Password: ")
    console.writer().flush()
    val scanner = new Scanner(console.reader())
    val password = scanner.nextLine()
    console.writer().flush()
    console.writer().println()
    SSH("nightmare.cs.uct.ac.za", HostConfig(
      login = PasswordLogin("jwatson", password), connectTimeout = Some(5000)
    )) { ssh =>
      println("connected to nightmare.cs.uct.ac.za")
      ssh.exec("mkdir -p ~/public_html/js")
      val scp = ssh.client.newSCPFileTransfer()
      val upload = scp.newSCPUploadClient()
      println(s"uploading ${js.allCode.size} file${if (js.allCode.size == 1) "" else "s"}...")
      for (jsFile <- js.allCode) {
        upload.copy(new FileSystemFile(jsFile.path), "/home/jwatson/public_html/js/" + jsFile.name)
        println(s"${jsFile.name} -> /home/jwatson/public_html/js/${jsFile.name}")
      }
      ssh.close()
    }
  }
}