name := "dijkstra"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.11.6"

scalacOptions ++= Seq("-Xlint", "-unchecked", "-deprecation", "-feature")

libraryDependencies += "de.ummels" %% "scala-prioritymap" % "0.3.0"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.2"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"
