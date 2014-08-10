addSbtPlugin("org.scala-lang.modules.scalajs" % "scalajs-sbt-plugin" % "0.5.3")

addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.6.0")

resolvers += "spray repo" at "http://repo.spray.io"

libraryDependencies += "com.decodified" %% "scala-ssh" % "0.6.4"