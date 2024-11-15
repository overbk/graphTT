val scala3Version = "3.5.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "graph-termination-tool",
    scalaVersion := scala3Version,
  )

scalacOptions := Seq("-Werror", "-Wunused:imports", "-unchecked", "-deprecation", "-Vprofile")

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "2.8.0",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "2.2.0",
  "org.scalacheck" %% "scalacheck" % "1.18.1" % "test"
)

inThisBuild( // scalafix configuration
  List(
    scalaVersion := scala3Version,
    semanticdbEnabled := true,
    semanticdbVersion := scalafixSemanticdb.revision
  )
)