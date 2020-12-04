ivyLoggingLevel := UpdateLogging.Quiet
scalacOptions in Compile ++= Seq("-feature", "-deprecation", "-Xfatal-warnings")

classpathTypes += "maven-plugin"

addSbtPlugin("io.github.davidgregory084" % "sbt-tpolecat" % "0.1.14")
addSbtPlugin("ch.epfl.scala" % "sbt-scalafix" % "0.9.19")
addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.4.0")
