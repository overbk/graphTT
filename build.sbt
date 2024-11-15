val scala3Version = "3.5.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "graph-termination-tool",
    scalaVersion := scala3Version,
  )

scalacOptions := Seq("-Werror", "-unchecked", "-deprecation", "-Vprofile")

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "2.8.0",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "2.2.0",
  "org.scalacheck" %% "scalacheck" % "1.18.1" % "test"
)
