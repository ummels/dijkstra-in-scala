name := "dijkstra"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.11.7"

scalacOptions ++= Seq("-Xlint", "-unchecked", "-deprecation", "-feature")

libraryDependencies += "de.ummels" %% "scala-prioritymap" % "0.3.0"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.5"

libraryDependencies += "com.storm-enroute" %% "scalameter-core" % "0.7"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.5" % "test"
