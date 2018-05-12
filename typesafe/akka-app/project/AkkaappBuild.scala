import sbt._
import sbt.Keys._

object AkkaappBuild extends Build {

  lazy val akkaapp = Project(
    id = "akka-app",
    base = file("."),
    settings = Project.defaultSettings ++ Seq(
      name := "akka-app",
      organization := "org.typesage",
      version := "0.1-SNAPSHOT",
      scalaVersion := "2.9.2",
      resolvers += "Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases",
      libraryDependencies += "com.typesafe.akka" % "akka-actor" % "2.0.1"
    )
  )
}
