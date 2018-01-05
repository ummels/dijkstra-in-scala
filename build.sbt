lazy val root = (project in file("."))
  .settings(
    name := "dijkstra",
    version := "1.0-SNAPSHOT",
    scalaVersion := "2.12.4",
    scalacOptions ++= Seq("-Xlint", "-unchecked", "-deprecation", "-feature"),
    autoAPIMappings := true,
    libraryDependencies += "de.ummels" %% "scala-prioritymap" % "1.0.0",
    libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.4",
    libraryDependencies += "com.storm-enroute" %% "scalameter-core" % "0.8.2",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % Test
  )
