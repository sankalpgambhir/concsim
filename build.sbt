inThisBuild(
  Def.settings(
    organization := "ch.epfl.lara",
    organizationName := "LARA",
    organizationHomepage := Some(url("https://lara.epfl.ch")),
    licenses := Seq("Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0.html")),
    versionScheme := Some("semver-spec"),
    scalacOptions ++= Seq(
      "-feature",
      "-deprecation",
      "-unchecked"
    ),
    javacOptions ++= Seq("-encoding", "UTF-8"),
    semanticdbEnabled := true,
    semanticdbVersion := scalafixSemanticdb.revision,
    scalafixDependencies += "com.github.liancheng" %% "organize-imports" % "0.6.0"
  )
)

val scala3Version = "3.2.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "concsim",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test,

    libraryDependencies += ("com.propensive" %% "escritoire-core" % "0.3.0").cross(CrossVersion.for3Use2_13),
    
    libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4",
  )
