import sbt._

object LibraryDependencies {

  val console4cats = "dev.profunktor" %% "console4cats" % "0.8.1"
  val log4CatsSlf4j = "io.chrisdavenport" %% "log4cats-slf4j" % "1.1.1"
  val scalafixOrganizeImports = "com.github.liancheng" %% "organize-imports" % "0.4.0"

  object log4j2 {
    private val log4j2Version = "2.13.3"
    val core = "org.apache.logging.log4j" % "log4j-core" % log4j2Version
    val api = "org.apache.logging.log4j" % "log4j-api" % log4j2Version
    val slf4j = "org.apache.logging.log4j" % "log4j-slf4j-impl" % log4j2Version
  }

  object Fs2 {
    private val fs2Version = "2.4.6"
    val core = "co.fs2" %% "fs2-core" % fs2Version
    val io = "co.fs2" %% "fs2-io" % "2.4.6"
  }

  object Cats {
    private val catsVersion = "2.3.0"
    val core = "org.typelevel" %% "cats-core" % catsVersion
    val effect = "org.typelevel" %% "cats-effect" % catsVersion
  }

  object Compiler {
    val kindProjector =
      ("org.typelevel" %% "kind-projector" % "0.11.2").cross(CrossVersion.full)
    val contextApplied = "org.augustjune" %% "context-applied" % "0.1.4"
  }

}
