val scala3Version = "3.5.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "graph-termination-tool",
    scalaVersion := scala3Version,
  )

scalacOptions := Seq("-Werror", "-unchecked", "-deprecation", "-Vprofile")

libraryDependencies += "org.typelevel" %% "cats-core" % "2.8.0"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.15"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.15" % "test"
libraryDependencies += "org.scalatestplus" %% "scalacheck-1-17" % "3.2.15.0" % "test"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.2.0"
