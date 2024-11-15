val scala3Version = "3.3.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "graph-termination-tool",
    scalaVersion := scala3Version,
  )

scalacOptions := Seq("-unchecked", "-deprecation", "-Vprofile") //"-Werror"

libraryDependencies += "org.typelevel" %% "cats-core" % "2.8.0"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.15"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.15" % "test"
libraryDependencies += "org.scalatestplus" %% "scalacheck-1-17" % "3.2.15.0" % "test"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.2.0"


import ReleaseTransformations._

// sbt-release configuration

releaseVersionBump := sbtrelease.Version.Bump.Next
releaseVersionFile := baseDirectory.value / "version.sbt"

publishConfiguration := publishConfiguration.value.withOverwrite(true)
releaseIgnoreUntrackedFiles := true

releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies,              // : ReleaseStep
  inquireVersions,                        // : ReleaseStep
  runClean,                               // : ReleaseStep
  runTest,                                // : ReleaseStep
  setReleaseVersion,                      // : ReleaseStep
  commitReleaseVersion,                   // : ReleaseStep, performs the initial git checks
  tagRelease,                             // : ReleaseStep
  setNextVersion,                         // : ReleaseStep
  commitNextVersion,                      // : ReleaseStep
  pushChanges                             // : ReleaseStep, also checks that an upstream branch is properly configured
)


