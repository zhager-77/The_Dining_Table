ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.8.3"

lazy val root = (project in file("."))
  .settings(
    name := "The_Dining_Table"
  )

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.20" % Test

libraryDependencies += "org.scalatestplus" %% "scalacheck-1-19" % "3.2.20.0" % Test

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "2.13.0",
  "org.typelevel" %% "cats-effect" % "3.6.3"
)