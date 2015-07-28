package de.ummels.dijkstra

import org.scalameter._

object Main extends App {
  val nodes = 5000
  val outDegree = 100

  val g = Graphs.randomGraph(nodes, outDegree, 10)

  for(dijkstra <- Seq(DijkstraMutable, DijkstraImmutable, DijkstraPriority)) {
    println(s"Running $dijkstra on random graph with $nodes nodes...")
    val time = measure {
      dijkstra.dijkstra(g)(0)
    }
    println(s"Total time: ${time.toInt} ms")
  }
}
