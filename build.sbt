


inThisBuild(nocomma {
  scalaVersion := "3.1.3"
  version := "0-SNAPSHOT"
  organization := "net.thecoda"
  organizationName := "github.com/thecoda"
  scalacOptions ++= Seq(
    // "-Xlint"
    // "-Xfatal-warnings",
    // , "-Xlog-implicits"
    //"-Ydebug",
    "-Xcheck-macros",
    "-language:implicitConversions",
    "-language:existentials",
    "-feature",
    "-deprecation", //hard to handle when supporting multiple scala versions...
    "-explain"
  )
})

lazy val root =
  project
    .in(file("."))
    .settings(nocomma {
      name := "macro-trials"
      resolvers ++= Dependencies.resolvers
      libraryDependencies ++= Dependencies.common
    })
