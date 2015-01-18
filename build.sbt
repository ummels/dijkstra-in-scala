name := "dijkstra"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.11.5"

scalacOptions ++= Seq("-Xlint", "-unchecked", "-deprecation", "-feature")

libraryDependencies += "de.ummels" %% "scala-prioritymap" % "0.2.0"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.2" % "test"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.1"
