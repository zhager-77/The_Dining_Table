ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.8.4"

lazy val root = (project in file("."))
  .settings(
    name := "The_Dining_Table"
  )

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.20" % Test

libraryDependencies += "org.scalatestplus" %% "scalacheck-1-19" % "3.2.20.0" % Test

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "2.13.0",
  "org.typelevel" %% "cats-effect" % "3.7.0",

  "org.typelevel"    %% "log4cats-core"   % "2.7.0",
  "org.typelevel"    %% "log4cats-slf4j"  % "2.7.0",
  "ch.qos.logback"    % "logback-classic" % "1.5.6"
)