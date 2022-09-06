import sbt._

object Dependencies {
  object V {
    val scalaTest = "3.2.12"
    val shapeless3 = "3.0.1"
    val slf4j = "1.7.36"
  }

  val common: Seq[ModuleID] = Seq(
    "org.slf4j" % "slf4j-nop" % V.slf4j % Test,
    "org.scalatest" %% "scalatest-shouldmatchers" % V.scalaTest % Test,
    "org.scalatest" %% "scalatest-wordspec" % V.scalaTest % Test,
    "org.scalatest" %% "scalatest-funspec" % V.scalaTest % Test,
    // "org.typelevel" %% "shapeless3-deriving" % V.shapeless3
  )

  val resolvers: Seq[Resolver] = Seq(
    "Apache public".at("https://repository.apache.org/content/groups/public/"),
    Resolver.mavenLocal
  )
}
